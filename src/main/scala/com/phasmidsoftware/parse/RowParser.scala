/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.{Header, Row}

import scala.annotation.implicitNotFound
import scala.util.Try
import scala.util.matching.Regex

/**
  * Trait to describe a parser which will yield a Try[Row] from a String representing a line of a table.
  *
  * @tparam Row the (parametric) Row type for which there must be evidence of a RowParser[Row, Input].
  * @tparam Input the (parametric) Input type for which there must be evidence of a RowParser[Row, Input].
  */
@implicitNotFound(msg = "Cannot find an implicit instance of RowParser[${Row}, ${Input}]. Typically, you might define a StandardRowParser or StandardStringsParser")
trait RowParser[Row, Input] {

  /**
    * Parse the Input, resulting in a Try[Row]
    *
    * @param x      the Input to be parsed.
    * @param header the header already parsed.
    * @return a Try[Row].
    */
  def parse(x: Input)(header: Header): Try[Row]

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

trait StringParser[Row] extends RowParser[Row, String]

/**
  * StandardRowParser: a parser which extends RowParser[Row] and will yield a Try[Row] from a String representing a line of a table.
  *
  * @param parser the LineParser to use
  * @tparam Row the (parametric) Row type for which there must be evidence of a CellParser[Row].
  */
case class StandardRowParser[Row: CellParser](parser: LineParser) extends StringParser[Row] {

  override def parse(w: String)(header: Header): Try[Row] = for (ws <- parser.parseRow(w); r <- RowValues(Row(ws, header)).convertTo[Row]) yield r

  override def parseHeader(w: String): Try[Header] = for (ws <- parser.parseRow(w.toUpperCase)) yield Header(ws)
}

object StandardRowParser {
  def apply[Row: CellParser](implicit rowConfig: RowConfig): StandardRowParser[Row] = StandardRowParser(LineParser.apply)

  // CONSIDER eliminating as this is never used.
  def apply[Row: CellParser](delimiter: Regex, string: Regex, enclosures: String, listSeparator: Char, quote: Char): StandardRowParser[Row] =
    StandardRowParser(new LineParser(delimiter, string, enclosures, listSeparator, quote))
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

  override def parse(ws: Strings)(header: Header): Try[Row] = RowValues(Row(ws, header)).convertTo[Row]

  override def parseHeader(ws: Strings): Try[Header] = Try(Header(ws map (_.toUpperCase())))
}
