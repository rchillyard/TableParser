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
   * NOTE: the expressions "end of input expected" and "end of input" are defined in (trait) Parsers:
   * def phrase[T](p: Parser[T]): Parser[T]
   * def acceptMatch[U]...
   * def acceptIf...
   * It's a shame that they didn't make it a constant in Parsers!
   *
   * CONSIDER checking that sequential lines really are sequential.
   * Currently, no use is made of `indexedString._2`
   *
   * @param indexedString a tuple of String and Int denoting the line and its index in the file.
   * @return a Try[Strings].
   */
  def parseRow(indexedString: (String, Int)): Try[Strings] =
    parseAll(row, indexedString._1) match {
      case Success(s, _) =>
        scala.util.Success(s)
      case Failure("end of input", _) | Failure("end of input expected", _) =>
        scala.util.Failure(MultiLineException(indexedString))
      case Failure(x, _) =>
        scala.util.Failure(formException(indexedString, x))
      case Error(x, _) =>
        scala.util.Failure(formException(indexedString, x))
  }

  /**
   * A parser for parsing a single row consisting of multiple cells separated by a delimiter.
   *
   * This parser matches and extracts repetitions of cells (parsed by the `cell` parser) separated by the specified
   * delimiter. The result is returned as a sequence of strings (`Strings`).
   *
   * - The `rep1sep` combinator ensures that at least one cell is expected in the row.
   * - The delimiter parser defines the separator between cells.
   *
   * This parser can be used in conjunction with other parsers to process row-based data with a specific format or structure.
   */
  lazy val row: Parser[Strings] =
    rep1sep(cell, delimiter)

  /**
   * A `Parser` that attempts to parse a cell value in a variety of forms.
   *
   * The `cell` parser evaluates different parsing strategies in sequence:
   *  - `quotedString`: Parses a string enclosed in quotation marks.
   *  - `list`: Parses a list structure.
   *  - `string`: Parses a generic string.
   *  - `failure("invalid string")`: Returns a parsing failure with the message "invalid string"
   *    if none of the other parsers are successful.
   *
   * This parser is declared as `lazy` to allow it to be initialized when first accessed,
   * enabling any dependencies it might have to be resolved at runtime.
   */
  lazy val cell: Parser[String] =
    quotedString | list | string | failure("invalid string")

  /**
   * A parser for quoted strings in the input, using a combination of possible parsing strategies.
   *
   * The `quotedString` method combines three different parsers to handle quoted strings:
   * 1. `quotedStringWithQuotes`: Parses quoted strings that include surrounding quotation marks.
   * 2. `pureQuotedString`: Parses quoted strings but may apply stricter conditions (e.g., specific delimiters or special characters).
   * 3. `failure("invalid quoted string")`: Ensures that any string not following the valid quoted string format results in a failure.
   *
   * This is defined as a lazy val, meaning the parser's initialization is deferred until it is first accessed.
   *
   * @return A `Parser[String]` that matches valid quoted strings in the input.
   */
  lazy val quotedString: Parser[String] =
    quotedStringWithQuotes | pureQuotedString | failure("invalid quoted string")

  /**
   * A `Parser` that extracts a quoted string from the input.
   *
   * The parser is defined as a combination of three elements:
   * - `quote`: Matches the opening quote character.
   * - `stringInQuotes`: Parses the content inside the quotes.
   * - `quote`: Matches the closing quote character.
   *
   * This parser uses the sequence combinators (`~>` and `<~`) to drop the quote characters themselves.
   *
   * @return a `Parser[String]` that successfully parses a string enclosed in quotes
   *         and returns the content within the quotes as a `String`.
   */
  lazy val pureQuotedString: Parser[String] =
    quote ~> stringInQuotes <~ quote

  /**
   * A `Parser` that matches a string that includes zero or more characters (other than quote characters as defined by the `quote` parameter).
   *
   * This parser specifically matches a sequence of characters that does not include the quote character (`"`).
   * The `quote` character in this context is expected to be defined elsewhere in the class or inherited scope.
   *
   * Usage:
   * This parser can be used to identify and extract substrings within a larger input that are enclosed in quotes,
   * as part of a more complex parsing process.
   *
   * @return a string that matches the defined regular expression.
   */
  lazy val stringInQuotes: Parser[String] =
    s"""[^$quote]*""".r

  /**
   * Parser for quoted strings that includes the surrounding quotes as part of the result.
   *
   * This lazy value utilizes `quotedStringWithQuotesAsList`, which parses the string components
   * into a list of strings. It then combines those components into a single string by adding
   * the surrounding quotes (as defined by the `quote` variable).
   *
   * The resulting parsed string includes both the content of the quoted string and the enclosing
   * quote characters.
   *
   * For example:
   * Input: `"Hello, World!"`
   * Output: `"Hello, World!"` (as a single string, including the quotation marks)
   *
   * @return a parser that yields the entire quoted string, including the enclosing quotes.
   */
  lazy val quotedStringWithQuotes: Parser[String] =
    quotedStringWithQuotesAsList ^^ (ws => ws.mkString(s"$quote"))

  /**
   * A parser that extracts a sequence of strings (Strings) from a quoted string,
   * interpreting quotes and escaped quotes appropriately.
   *
   * The input is expected to start and end with a quote character.
   * Inside the quotes, delimited substrings can be separated by pairs of quote characters.
   *
   * Example Input: "abc""def""ghi"
   * Parsed Output: Seq("abc", "def", "ghi")
   *
   * The parser works as follows:
   * - `quote ~>` ensures that the input starts with a single opening quote.
   * - `repsep(stringInQuotes, s"$quote$quote")` parses a repetition of strings defined by `stringInQuotes`,
   * with consecutive strings separated by double quotes (`""`).
   * - `<~ quote` ensures the input ends with a single closing quote.
   *
   * @return a Parser[Strings] capable of parsing a quoted string into a sequence of substrings.
   */
  lazy val quotedStringWithQuotesAsList: Parser[Strings] =
    quote ~> repsep(stringInQuotes, s"$quote$quote") <~ quote

  /**
   * A lazy val for a Parser that parses a list-like structure encapsulated by open and close characters.
   *
   * The parser works as follows:
   * 1. It expects an opening character (`getOpenChar`).
   * 2. It parses a component followed by a `listSeparator`, and one or more components separated by `listSeparator` using `rep1sep`.
   * 3. It concludes by expecting a closing character (`getCloseChar`).
   * 4. Finally, it transforms the parsed elements into a String representation where the components are encapsulated by curly braces
   * and separated by commas, e.g., "{component1,component2,...}".
   *
   * @return A `Parser[String]` that produces the string representation of the parsed list.
   */
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

    //        check(cell, "http://www.imdb.com/title/tt0499549/?ref_=fn_tt_tt_1", "http://www.imdb.com/title/tt0499549/?ref_=fn_tt_tt_1") &&
    (check(cell, "Hello", "Hello") && check(quotedString, s"""${quote}Hello${getDelimiterChar}Goodbye$quote""", s"""Hello${getDelimiterChar}Goodbye""")).squawk()
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