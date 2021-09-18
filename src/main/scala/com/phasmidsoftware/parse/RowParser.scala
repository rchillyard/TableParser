/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.{Header, Row}

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
    * Parse the Input, resulting in a Try[Header]
    *
    * CONSIDER making this share the same signature as parse but for different Row type.
    *
    * @param x the Input to be parsed.
    * @return a Try[Header]
    */
  def parseHeader(x: Input): Try[Header]
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

  /**
    * Method to parse a String and return a Try[Row].
    *
    * @param indexedString      the row and index as a (String., Int)
    * @param header the header already parsed.
    * @return a Try[Row].
    */
  def parse(indexedString: (String, Int))(header: Header): Try[Row] = for (ws <- parser.parseRow(indexedString); r <- RowValues(Row(ws, header, indexedString._2)).convertTo[Row]) yield r

  /**
    * Method to parse a String as a Try[Header].
    *
    * @param w the header row as a String.
    * @return a Try[Header].
    */
  def parseHeader(w: String): Try[Header] = for (ws <- parser.parseRow((w, -1))) yield Header(ws)
}

object StandardRowParser {
  def apply[Row: CellParser](implicit rowConfig: RowConfig): StandardRowParser[Row] = StandardRowParser(LineParser.apply)
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
    *
    * @param indexedString      the rows and index as a (Strings., Int)
    * @param header the header already parsed.
    * @return a Try[Row].
    */
  def parse(indexedString: (Strings, Int))(header: Header): Try[Row] = RowValues(Row(indexedString._1, header, indexedString._2)).convertTo[Row]

  /**
    * Method to parse a sequence of Strings as a Try[Header].
    *
    * @param ws the header row as a sequence of Strings.
    * @return a Try[Header].
    */
  def parseHeader(ws: Strings): Try[Header] = Try(Header(ws))
}
