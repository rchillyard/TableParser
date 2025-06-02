/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.parse

import org.slf4j.{Logger, LoggerFactory}
import scala.annotation.tailrec
import scala.util.Try
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * LineParser: class to parse lines of a CSV file.
 * NOTE: list elements always appear as a string in the form { element0 , element1 , ... }
 *
 * @param delimiter     a Regex used to match a delimiter between cells in a row.
 * @param string        a Regex used to match the content of a cell.
 * @param enclosures    the enclosure characters around a list (if any).
 * @param listSeparator the list separator character.
 * @param quote         the quote character which is able to preempt the string regex:
 *                      between two quote characters,
 *                      there can be any number of any character (other than quote).
 * @param verbose       will print the various parameters.
 */
class LineParser(delimiter: Regex, string: Regex, enclosures: String, listSeparator: Char, quote: Char, verbose: Boolean = false) extends JavaTokenParsers {

  if (verbose) LineParser.logger.info(s"delimiter: '${delimiter.regex}', string: '${string.regex}', enclosures: '$enclosures', quote: '$quote', listSeparator: '$listSeparator', ")
  runChecks()

  override def skipWhitespace: Boolean = false

  /**
   * Method to parse a Row.
   *
   * NOTE: the expression "end of input expected" must be the same as the failure defined in (trait) Parsers: def phrase[T](p: Parser[T]): Parser[T]
   * It's a shame that they didn't make it a constant in Parsers!
   *
   * @param indexedString a tuple of String and Int denoting the line and its index in the file.
   * @return a Try[Strings].
   */
  def parseRow(indexedString: (String, Int)): Try[Strings] =
    parseAll(row, indexedString._1) match {
      case Success(s, _) =>
        scala.util.Success(s)
      case Failure("end of input expected", _) =>
        scala.util.Failure(MultiLineException(indexedString))
      case Failure(x, _) =>
        scala.util.Failure(formException(indexedString, x))
      case Error(x, _) =>
        scala.util.Failure(formException(indexedString, x))
  }

  lazy val row: Parser[Strings] =
    rep1sep(cell, delimiter)

  lazy val cell: Parser[String] =
    quotedString | list | string | failure("invalid string")

  lazy val quotedString: Parser[String] =
    quotedStringWithQuotes | pureQuotedString | failure("invalid quoted string")

  private lazy val pureQuotedString: Parser[String] =
    quote ~> stringInQuotes <~ quote

  private lazy val stringInQuotes: Parser[String] =
    s"""[^$quote]*""".r

  lazy val quotedStringWithQuotes: Parser[String] =
    quotedStringWithQuotesAsList ^^ (ws => ws.mkString(s"$quote"))

  lazy val quotedStringWithQuotesAsList: Parser[Strings] =
    quote ~> repsep(stringInQuotes, s"$quote$quote") <~ quote

  lazy val list: Parser[String] =
    getOpenChar ~> (component ~ listSeparator ~ rep1sep(component, listSeparator)) <~ getCloseChar ^^ { case x ~ _ ~ xs => (x +: xs).mkString("{", ",", "}") }

  private val regexComponent =
    s"""[^,$listSeparator}]+"""
  private lazy val component: Parser[String] =
    regexComponent.r

  private lazy val getOpenChar: Parser[String] =
    s"${enclosures.headOption.getOrElse("")}"

  private lazy val getCloseChar: Parser[String] =
    s"${enclosures.lastOption.getOrElse("")}"

  private def formException(indexedString: (String, Int), x: String) =
    ParserException(s"Cannot parse row ${indexedString._2}: '${indexedString._1}' due to: $x")

  // XXX used only for debugging
  override def toString: String =
    s"""LineParser: delimiter=$delimiter, string=$string, listSeparator='$listSeparator', enclosures='$enclosures', quote="$quote""""

  private lazy val getDelimiterChar: Char = {
    @tailrec
    def inner(w: Seq[Char], escaped: Boolean): Char =
      w match {
        case h :: t =>
          if (escaped) h match {
            case 't' => '\t'
            case '\\' => '\\'
            case 'n' => '\n'
            case 'r' => '\r'
            case 'd' => '0'
            case 'f' => '\f'
            case 'b' => '\b'
            case _ => h
          }
          else h match {
            case '[' | '{' => inner(t, escaped = false)
            case '\\' => inner(t, escaped = true)
            case '^' => throw ParserException(s"Cannot get a delimiter from ${delimiter.regex} (unsupported)")
            case _ => h
          }
        case Nil =>
          throw ParserException(s"Cannot get a delimiter from ${delimiter.regex}")
      }

    inner(delimiter.regex.toList, escaped = false)
  }

  private def runChecks(): Unit = {
    def check[X](parser: Parser[X], input: String, matchedValue: X) =
      Try(parseAll(parser, input) match {
      case Success(`matchedValue`, _) =>
      // XXX do nothing
      case Success(z, _) =>
        throw ParserException(s"Warning: (LineParser constructor validity check): '$input' did not result in $matchedValue but instead matched: $z")
      case Failure(z, _) =>
        throw ParserException(s"Warning: (LineParser constructor validity check): '$input' did not result in $matchedValue because: $z")
      case Error(z, _) =>
        throw ParserException(s"Warning: (LineParser constructor validity check): '$input' did not result in $matchedValue because: $z")
    })

    implicit class Trial(x: Try[Unit]) {
      def squawk(): Unit = x match {
        case scala.util.Failure(z) =>
          LineParser.logger.warn(s"squawk: $z")
        case _ =>
        // XXX do nothing
      }

      def &&(y: Trial): Trial =
        if (x.isSuccess) y else x
    }

    (
            check(cell, "Hello", "Hello") &&
                    //        check(cell, "http://www.imdb.com/title/tt0499549/?ref_=fn_tt_tt_1", "http://www.imdb.com/title/tt0499549/?ref_=fn_tt_tt_1") &&
                    check(quotedString, s"""${quote}Hello${getDelimiterChar}Goodbye$quote""", s"""Hello${getDelimiterChar}Goodbye""")
            ).squawk()
  }

}

/**
 * Companion object for the `LineParser` class, responsible for constructing instances of `LineParser`
 * and providing logging capabilities.
 */
object LineParser {
  /**
   * Constructs an instance of `LineParser` using an implicitly provided `RowConfig`.
   * The provided `RowConfig` instance defines various parameters required for parsing rows, such as delimiter,
   * string format, list enclosures, list separator, and quote characters.
   *
   * @param c an implicit instance of `RowConfig` that specifies configuration settings for the `LineParser`.
   *          This includes attributes such as `delimiter` (Regex for column separators),
   *          `string` (Regex for content within cells), `listEnclosure` (characters for enclosing lists),
   *          `listSep` (character separating elements in a list), and `quote` (character for quoting strings).
   * @return a new instance of `LineParser`, created with the parameters defined in the provided `RowConfig`.
   */
  def apply(implicit c: RowConfig): LineParser = {
    LineParser.logger.info(s"Constructing LineParser with an implicitly defined instance of RowConfig: $c")
    new LineParser(c.delimiter, c.string, c.listEnclosure, c.listSep, c.quote)
  }

  val logger: Logger = LoggerFactory.getLogger(LineParser.getClass)
}

/**
 * Represents an exception that occurs during parsing operations.
 *
 * @param msg the message describing the cause of the exception.
 * @param e   the underlying throwable that caused this exception (optional, defaults to null).
 */
case class ParserException(msg: String, e: Throwable = null) extends Exception(msg, e)

/**
 * The `MultiLineException` class is a case class that extends the `Exception` class.
 * This exception is designed to handle multi-line exception scenarios where custom content is provided to describe the error.
 *
 * @param x The content of type `X` that describes the exception or provides context for it.
 * @tparam X The generic type of the content that can represent the cause or context of the exception.
 *
 *           The exception message is constructed dynamically by concatenating the string "multi-line exception: " with the provided content.
 *
 *           Usage example:
 * {{{
 * val exception = MultiLineException("An error occurred")
 * throw exception
 * }}}
 */
case class MultiLineException[X](x: X) extends Exception(s"multi-line exception: $x")