/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import cats.effect.IO
import com.phasmidsoftware.flog.Flog
import com.phasmidsoftware.table.{Header, Row}
import com.phasmidsoftware.util.FP
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
   * Parse the Input, resulting in a Try[Row]
   *
   * @param indexedRow the Input, and its index to be parsed.
   * @param header     the header already parsed.
   * @return a Try[Row].
   */
  def parse(indexedRow: (Input, Int))(header: Header): Try[Row]

  /**
   * Parse the Input, resulting in a IO[Header]
   *
   * CONSIDER making this share the same signature as parse but for different Row type.
   *
   * @param xs a sequence of Inputs to be parsed.
   * @return a IO[Header]
   */
  def parseHeader(xs: Seq[Input]): IO[Header]
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

  val flog = Flog[StandardRowParser[_]]

  import flog._

  /**
   * Method to parse a String and return a Try[Row].
   *
   * @param indexedString the row and index as a (String., Int)
   * @param header        the header already parsed.
   * @return a Try[Row].
   */
  def parse(indexedString: (String, Int))(header: Header): Try[Row] =
    for {
      ws <- s"parsed row from $indexedString" !! parser.parseRow(indexedString)
      r <- s"after conversion" !! doConversion(indexedString, header, ws)
    } yield r

  /**
   * Method to parse a String as a IO[Header].
   *
   * @param xs the header row(s) as a String.
   * @return a IO[Header].
   */
  def parseHeader(xs: Strings): IO[Header] = {
    val wsys: Seq[Try[Strings]] = for (x <- xs.tail) yield parser.parseRow(x, -1)
    IO.fromTry(for (w <- Try(xs.head); ws <- parser.parseRow((w, -1)); wss <- FP.sequence(wsys)) yield Header(ws, wss))
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
   * Method to parse a sequence of String into a Try[Row].
   *
   * @param indexedString the rows and index as a (Strings., Int)
   * @param header        the header already parsed.
   * @return a Try[Row].
   */
  def parse(indexedString: (Strings, Int))(header: Header): Try[Row] =
    RowValues(Row(indexedString._1, header, indexedString._2)).convertTo[Row]

  /**
   * Method to parse a sequence of Strings as a IO[Header].
   *
   * @param ws the header row as a sequence of Strings.
   * @return a IO[Header].
   */
  def parseHeader(ws: Seq[Strings]): IO[Header] = IO(Header(ws))
}
