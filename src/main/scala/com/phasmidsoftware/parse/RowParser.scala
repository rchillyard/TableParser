package com.phasmidsoftware.parse

import com.phasmidsoftware.table.{Header, Row}

import scala.util.Try
import scala.util.matching.Regex

/**
  * Trait to describe a parser which will yield a Try[Row] from a String representing a line of a table.
  *
  * @tparam Row the (parametric) Row type for which there must be evidence of a CellParser[Row].
  */
trait RowParser[Row] {

  /**
    * Parse the String w, resulting in a Try[Row]
    *
    * @param w      the String to be parsed.
    * @param header the header already parsed.
    * @return a Try[Row].
    */
  def parse(w: String)(header: Header): Try[Row]

  /**
    * Parse the String, resulting in a Seq[String]
    *
    * @param w the String to be parsed: always the first line of a CSV file.
    * @return a Try[Seq[String]
    */
  def parseHeader(w: String): Try[Header]
}

/**
  * StandardRowParser: a parser which extends RowParser[Row] and will yield a Try[Row] from a String representing a line of a table.
  *
  * @param parser the LineParser to use
  * @tparam Row the (parametric) Row type for which there must be evidence of a CellParser[Row].
  */
case class StandardRowParser[Row: CellParser](parser: LineParser) extends RowParser[Row] {

  override def parse(w: String)(header: Header): Try[Row] = for (ws <- parser.parseRow(w); r <- Try(RowValues(Row(ws, header)).convertTo[Row])) yield r

  override def parseHeader(w: String): Try[Header] = for (ws <- parser.parseRow(w.toUpperCase)) yield Header(ws)
}

object StandardRowParser {
  def apply[Row: CellParser](delimiter: Regex, string: Regex, enclosures: String, listSeparator: Char, quote: Char): StandardRowParser[Row] =
    StandardRowParser(new LineParser(delimiter, string, enclosures, listSeparator, quote))
}

trait RowConfig {
  /**
    * the delimiter Regex (see LineParser). defaults to ", *".r, i.e. a comma followed by any n=umber of spaces.*
    */
  val delimiter: Regex
  /**
    * the "string" Regex (see LineParser). defaults to "\w+".r, i.e. at least one word character.
    */
  val string: Regex
  /**
    * the "listSep" character (see LineParser). defaults to "|"
    */
  val listSep: Char
  /**
    * the "listEnclosure" characters (see LineParser). defaults to "{}"
    */
  val listEnclosure: String
  /**
    * the "quote" Char (see LineParser). defaults to ".
    */
  val quote: Char

  override def toString: String = s"RowConfig: delimiter='$delimiter', string='$string', listSep='$listSep', listEnclosure='$listEnclosure', $quote='$quote'"
}

trait DefaultRowConfig extends RowConfig {
  /**
    * the delimiter Regex (see LineParser). defaults to ", *".r, i.e. a comma followed by any n=umber of spaces.*
    */
  val delimiter: Regex = ", *".r
  /**
    * the "string" Regex (see LineParser). defaults to "\w+".r, i.e. at least one word character.
    */
  val string: Regex = """\w*""".r

  /**
    * the "listSep" character (see LineParser). defaults to "|"
    */
  override val listSep: Char = '|'
  /**
    * the "listEnclosure" characters (see LineParser). defaults to "{}"
    */
  override val listEnclosure: String = "{}"
  /**
    * the "quote" Char (see LineParser). defaults to ".
    */
  val quote: Char = '"'
}


object RowConfig {

  implicit object defaultRowConfig extends DefaultRowConfig

}
