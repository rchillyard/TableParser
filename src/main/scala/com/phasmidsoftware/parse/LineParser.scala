package com.phasmidsoftware.parse

import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * LineParser: class to parse lines of a CSV file.
  * NOTE: list elements always appear as a string in the form { element0 , element1 , ... }
  *
  * @param delimiter a Regex used to match a delimiter between cells in a row.
  * @param string    a Regex used to match the content of a cell.
  *                  @param enclosures the enclosure characters around a list (if any).
  *                                    @param listSeparator the list separator character.
  * @param quote     the quote character which is able to preempt the string regex:
  *                  between two quote characters,
  *                  there can be any number of any character (other than quote).
  */
class LineParser(delimiter: Regex, string: Regex, enclosures: String, listSeparator: Char, quote: Char) extends JavaTokenParsers {
  override def skipWhitespace: Boolean = false

  def parseRow(w: String): Try[Seq[String]] = parseAll(row, w) match {
    case Success(s, _) => scala.util.Success(s)
    case x => scala.util.Failure(ParserException(x.toString))
  }

  def row: Parser[Seq[String]] = rep1sep(cell, delimiter)

  def cell: Parser[String] = quotedString | list | string | failure("invalid string")

  def quotedString: Parser[String] = quote ~> s"""[^$quote]*""".r <~ quote

  def list: Parser[String] = getOpenChar ~> (component ~ listSeparator ~ rep1sep(component, listSeparator)) <~ getCloseChar ^^ { case x ~ s ~ xs => (x +: xs).mkString("{", ",", "}") }

  private def component: Parser[String] = s"""[^,$listSeparator}]+""".r

  override def toString: String = s"""LineParser: delimiter=$delimiter, string=$string, listSeparator='$listSeparator', enclosures='$enclosures', quote="$quote""""

  private def getOpenChar: Parser[String] = if (enclosures.nonEmpty) enclosures.head+"" else ""
  private def getCloseChar: Parser[String] = if (enclosures.nonEmpty) enclosures.last+"" else ""
}

object LineParser {
  def apply(implicit c: RowConfig): LineParser = new LineParser(c.delimiter, c.string, c.listEnclosure, c.listSep, c.quote)
}

case class ParserException(msg: String) extends Exception(msg)