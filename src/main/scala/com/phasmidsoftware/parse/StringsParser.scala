/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.{Header, Row}

import scala.util.Try

/**
  * Trait to describe a parser which will yield a Try[Row] from a sequence of Strings representing a row of a table.
  *
  * @tparam Row the (parametric) Row type for which there must be evidence of a CellParser[Row].
  */
trait StringsParser[Row] {

  /**
    * Convert the sequence of String into Try[Row]
    *
    * @param ws     the input Strings
    * @param header the header already parsed.
    * @return a Try[Row].
    */
  def parse(ws: Seq[String])(header: Header): Try[Row]

  /**
    * Convert the sequence of String into Try[Header]
    *
    * @param ws the input Strings: should be the first "line" of a table file.
    * @return a Try[Header]
    */
  def parseHeader(ws: Seq[String]): Try[Header]
}

/**
  * StandardRowParser: a parser which extends RowParser[Row] and will yield a Try[Row] from a String representing a line of a table.
  *
  * @tparam Row the (parametric) Row type for which there must be evidence of a CellParser[Row].
  */
case class StandardStringsParser[Row: CellParser]() extends StringsParser[Row] {

  override def parse(ws: Seq[String])(header: Header): Try[Row] = RowValues(Row(ws, header)).convertTo[Row]

  override def parseHeader(ws: Seq[String]): Try[Header] = Try(Header(ws map (_.toUpperCase())))
}

object StandardStringsParser {
  //  def apply[Row: CellParser](implicit rowConfig: RowConfig): StandardStringsParser[Row] = StandardStringsParser()
}
