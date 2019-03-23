package com.phasmidsoftware.parse

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * LineParser: class to parse lines of a CSV file.
  *
  * @param delimiter a Regex used to match a delimiter between cells in a row.
  * @param string    a Regex used to match the content of a cell.
  * @param quote     the quote character which is able to preempt the string regex:
  *                  between two quote characters,
  *                  there can be any number of any character (other than quote).
  */
class LineParser(delimiter: Regex, string: Regex, quote: Char) extends JavaTokenParsers {
  override def skipWhitespace: Boolean = false

  def parseRow(w: String): Try[Seq[String]] = parseAll(row, w) match {
    case Success(s, _) => scala.util.Success(s)
    case Failure(x, _) => scala.util.Failure(ParserException(x))
    case Error(x, _) => scala.util.Failure(ParserException(x))
  }

  def row: Parser[Seq[String]] = rep1sep(cell, delimiter)

  def cell: Parser[String] = quotedString | string | failure("invalid string")

  def quotedString: Parser[String] = quote ~> s"""[^$quote]*""".r <~ quote

  override def toString: String = s"""LineParser: delimiter=$delimiter, string=$string, quote="$quote""""
}

object LineParser {
  def apply(implicit c: RowConfig): LineParser = new LineParser(c.delimiter, c.string, c.quote)
}

case class ParserException(msg: String) extends Exception(msg)