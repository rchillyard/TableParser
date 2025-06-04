/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.table.{Header, Row}
import com.phasmidsoftware.tableparser.core.util.FP
import scala.annotation.implicitNotFound
import scala.util.Try

/**
 * Trait to describe a parser which will yield a Try[Row] from a String representing a row of a table.
 *
 * @tparam Row   the (parametric) Row type for which there must be evidence of a RowParser[Row, Input].
 * @tparam Input the (parametric) Input type for which there must be evidence of a RowParser[Row, Input].
 */
@implicitNotFound(msg = "Cannot find an implicit instance of RowParser[${Row}, ${Input}]. Typically, you might define a StandardRowParser or StandardStringsParser")
trait RowParser[Row, Input] {

  /**
   * Parses a row of input data into a Row object, using a specified Header for context.
   *
   * @param header     the Header object which provides metadata about the columns in the input.
   * @param indexedRow a tuple consisting of the input data (of generic type Input) and the row index (Int).
   * @return a Try that, on success, contains the parsed Row object. If parsing fails, the Try contains Failure with the appropriate exception.
   */
  def parse(header: Header)(indexedRow: (Input, Int)): Try[Row]

  /**
   * Parse the Input, resulting in a Try[Header]
   *
   * CONSIDER making this share the same signature as parse but for different Row type.
   *
   * @param xs a sequence of Inputs to be parsed.
   * @return a Try[Header]
   */
  def parseHeader(xs: Seq[Input]): Try[Header]
}

/**
 * A RowParser whose input type is String.
 *
 * @tparam Row the (parametric) Row type for which there must be evidence of a RowParser[Row, Input].
 */
trait StringParser[Row] extends RowParser[Row, String]

/**
 * StandardRowParser: a parser which extends RowParser[Row] and will yield a Try[Row] from a String representing a line of a table.
 *
 * @param parser the LineParser to use
 * @tparam Row the (parametric) Row type for which there must be evidence of a CellParser[Row].
 */
case class StandardRowParser[Row: CellParser](parser: LineParser) extends StringParser[Row] {

//  private val flog = Flog[StandardRowParser[_]]
//
//  import flog._

  /**
   * Parses a given indexed string (line and its index in a file) using a provided header and the internal row parser.
   * This method converts the parsed row of data (as Strings) into a well-defined `Row` object according to the header.
   *
   * @param header        the header object providing column information for the row.
   * @param indexedString a tuple containing the string representation of the line and its index in the file.
   * @return a `Try[Row]` representing the parsed and converted row, or a failure if the parsing or conversion fails.
   */
  def parse(header: Header)(indexedString: (String, Int)): Try[Row] =
    for {
      ws <- parser.parseRow(indexedString)
      r <- doConversion(indexedString, header, ws)
    } yield r

  /**
   * Method to parse a String as a Try[Header].
   *
   * @param xs the header row(s) as a String.
   * @return a Try[Header].
   */
  def parseHeader(xs: Strings): Try[Header] = {
    val wsys: Seq[Try[Strings]] = for (x <- xs.tail) yield parser.parseRow(x, -1)
    for (w <- Try(xs.head); ws <- parser.parseRow((w, -1)); wss <- FP.sequence(wsys)) yield Header(ws, wss)
  }

  private def doConversion(indexedString: (String, Int), header: Header, ws: Strings) =
    RowValues(Row(ws, header, indexedString._2)).convertTo[Row]
}

object StandardRowParser {
  def create[Row: CellParser](implicit rowConfig: RowConfig): StandardRowParser[Row] = StandardRowParser(LineParser.apply)
}

/**
 * Trait to describe a parser which will yield a Try[Row] from a sequence of Strings representing a row of a table.
 *
 * @tparam Row the (parametric) Row type for which there must be evidence of a CellParser[Row].
 */
trait StringsParser[Row] extends RowParser[Row, Strings]


/**
 * StandardRowParser: a parser which extends RowParser[Row] and will yield a Try[Row] from a String representing a line of a table.
 *
 * @tparam Row the (parametric) Row type for which there must be evidence of a CellParser[Row].
 */
case class StandardStringsParser[Row: CellParser]() extends StringsParser[Row] {

  /**
   * Parses a given indexed String (row data and its index) using the specified table header to produce a Try[Row].
   *
   * @param header        the table Header object providing metadata about the row's structure (e.g., column names).
   * @param indexedString a tuple where the first element is the sequence of Strings representing the row data,
   *                      and the second element is the index of the row in the table.
   * @return a Try[Row], which is a Row object if parsing is successful, or a failure if parsing encounters an error.
   */
  def parse(header: Header)(indexedString: (Strings, Int)): Try[Row] =
    RowValues(Row(indexedString._1, header, indexedString._2)).convertTo[Row]

  /**
   * Method to parse a sequence of Strings as a Try[Header].
   *
   * @param ws the header row as a sequence of Strings.
   * @return a Try[Header].
   */
  def parseHeader(ws: Seq[Strings]): Try[Header] = Try(Header(ws))
}
