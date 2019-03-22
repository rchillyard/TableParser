package com.phasmidsoftware.tableparser

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

trait RowParser[Row] {

  def parse(w: String)(header: Seq[String]): Try[Row]

  def parseHeader(w: String): Seq[String]
}

case class StandardRowParser[Row: CellParser](delimiter: Regex = ",".r, string: Regex = """\w*""".r, quote: Char = '"') extends RowParser[Row] {
  val parser = new TokenParser(delimiter, string, quote)

  override def parse(w: String)(header: Seq[String]): Try[Row] = for (ws <- parser.parseRow(w); r <- Try(RowValues(Row(ws, header)).convertTo[Row])) yield r

  override def parseHeader(w: String): Seq[String] = ???
}

case class Row(ws: Seq[String], hdr: Seq[String]) extends (String => String) {
  /**
    * Method to yield the value for a given column name
    *
    * @param w the column name
    * @return the value as a String
    * @throws IndexOutOfBoundsException if w is not contained in hdr or hdr is longer than ws.
    */
  override def apply(w: String): String = ws(hdr.indexOf(w))
}

class TokenParser(delimiter: Regex, string: Regex, quote: Char) extends JavaTokenParsers {
  def parseRow(w: String): Try[Seq[String]] = parseAll(row, w) match {
    case Success(s, _) => scala.util.Success(s)
    case Failure(x, _) => scala.util.Failure(ParserException(x))
    case Error(x, _) => scala.util.Failure(ParserException(x))
  }

  def row: Parser[Seq[String]] = rep1sep(cell, delimiter)

  def cell: Parser[String] = quotedString | string | failure("invalid string")

  def quotedString: Parser[String] = quote ~> s"""[^$quote]*""".r <~ quote
}

case class ParserException(msg: String) extends Exception(msg)