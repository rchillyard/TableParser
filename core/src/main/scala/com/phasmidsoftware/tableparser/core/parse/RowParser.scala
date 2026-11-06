/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.table.{Header, RawRow, Row}
import com.phasmidsoftware.tableparser.core.util.FP
import scala.annotation.implicitNotFound
import scala.util.{Failure, Success, Try}

/**
 * Trait to describe a parser which will yield a Try[Row] from a String representing a row of a table.
 *
 * @tparam Row   the (parametric) Row type for which there must be evidence of a RowParser[Row, Input].
 * @tparam Input the (parametric) Input type for which there must be evidence of a RowParser[Row, Input].
 */
@implicitNotFound(msg = "Cannot find an implicit instance of RowParser[${Row}, ${Input}]. Typically, you might define a StandardRowParser or StandardStringsParser")
trait RowParser[Row, Input]:

  /**
   * Parses the given input using the provided header and produces a result wrapped in a Try.
   *
   * @param header the header object that defines the schema or metadata for parsing the row.
   * @param input  the input of type Input to be parsed into a row of type Row.
   * @return a Try[Row], where Success contains the parsed row if parsing succeeds,
   *         or Failure contains an exception if parsing fails.
   */
  def parse(header: Header)(input: Input): Try[Row]

  /**
   * Parses an indexed row, using the given header, into a Try[Row].
   *
   * @param header     the header which defines the schema or metadata for parsing the row.
   * @param indexedRow a tuple containing the input row (of type Input) and its index (of type Int).
   * @return a Try[Row] which will be a Success containing the parsed row if parsing is successful,
   *         or a Failure with an exception if parsing fails.
   */
  def parseIndexed(header: Header)(indexedRow: (Input, Int)): Try[Row]

  /**
   * Method to parse an indexed row, using the given header, unless it fails a raw predicate:
   * the predicate is applied to the row's cells (as Strings) before they are converted, so a row which fails it
   * costs only the splitting of its cells.
   *
   * This default can't apply a raw predicate (it doesn't know how to split the input into cells),
   * so, given one, it fails rather than ignore it. StandardRowParser and StandardStringsParser can apply one.
   *
   * @param header            the header which defines the schema or metadata for parsing the row.
   * @param maybeRawPredicate an optional predicate on the row's raw cells (None: parse every row).
   * @param indexedRow        a tuple containing the input row (of type Input) and its index (of type Int).
   * @return a Try of Some(row) if the row was parsed, or None if it failed the raw predicate.
   */
  def parseIndexedIf(header: Header, maybeRawPredicate: Option[RawRow => Boolean])(indexedRow: (Input, Int)): Try[Option[Row]] =
    maybeRawPredicate match
      case None => parseIndexed(header)(indexedRow) map (Some(_))
      case Some(_) => Failure(ParserException(s"this RowParser (${getClass.getName}) can't apply a raw predicate"))

  /**
   * Parse the Input, resulting in a Try[Header]
   *
   * CONSIDER making this share the same signature as parse but for different Row type.
   *
   * @param xs a sequence of Inputs to be parsed.
   * @return a Try[Header]
   */
  def parseHeader(xs: Seq[Input]): Try[Header]

object RowParser:
  /**
   * Method to convert a row's cells (by evaluating conversion) only if they satisfy the raw predicate (if there is one).
   *
   * @param maybeRawPredicate an optional predicate on the row's raw cells.
   * @param header            the header.
   * @param ws                the row's cells.
   * @param conversion        the conversion of the cells to a row (evaluated only if wanted).
   * @tparam Row the row type.
   * @return a Try of Some(row), or Success(None) if the cells failed the raw predicate.
   */
  private[parse] def convertIf[Row](maybeRawPredicate: Option[RawRow => Boolean], header: Header, ws: Strings)(conversion: => Try[Row]): Try[Option[Row]] =
    if (maybeRawPredicate.forall(p => p(RawRow(ws, header)))) conversion map (Some(_)) else Success(None)

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
case class StandardRowParser[Row: CellParser](parser: LineParser) extends StringParser[Row]:

//  private val flog = Flog[StandardRowParser[_]]
//
//  import flog._

  /**
   * Parses a given indexed string (line and its index in a file) using a provided header and the internal row parser.
   * This method converts the parsed row of data (as Strings) into a well-defined `Row` object according to the header.
   *
   * @param header        the header object providing column information for the row.
   * @param indexedString a tuple containing the string representation of the line and its index in the file.
   * @return a `Try[Row]` representing the parsed and converted row, or a failure if the parsing or conversion fails.
   */
  def parseIndexed(header: Header)(indexedString: (String, Int)): Try[Row] =
    for {
      ws <- parser.parseRow(indexedString)
      r <- doConversion(indexedString, header, ws)
    } yield r

  /**
   * Parses a given indexed string (as parseIndexed does) unless its cells fail the raw predicate,
   * which is applied after the line has been split into cells, but before they are converted.
   *
   * @param header            the header object providing column information for the row.
   * @param maybeRawPredicate an optional predicate on the row's raw cells (None: parse every row).
   * @param indexedString     a tuple containing the string representation of the line and its index in the file.
   * @return a Try of Some(row) if the row was parsed, or None if it failed the raw predicate.
   */
  override def parseIndexedIf(header: Header, maybeRawPredicate: Option[RawRow => Boolean])(indexedString: (String, Int)): Try[Option[Row]] =
    for {
      ws <- parser.parseRow(indexedString)
      ro <- RowParser.convertIf(maybeRawPredicate, header, ws)(doConversion(indexedString, header, ws))
    } yield ro

  /**
   * Parses the given input using the provided header and produces a result wrapped in a Try.
   *
   * @param header the header object that defines the schema or metadata for parsing the row.
   * @param input  the input of type Input to be parsed into a row of type Row.
   * @return a Try[Row], where Success contains the parsed row if parsing succeeds,
   *         or Failure contains an exception if parsing fails.
   */
  def parse(header: Header)(input: String): Try[Row] =
    parseIndexed(header)(input -> 0) // NOTE this is a kluge in that we don't have a line sequence number so we just use 0 instead.

  /**
   * Method to parse a String as a Try[Header].
   *
   * @param xs the header row(s) as a String.
   * @return a Try[Header].
   */
  def parseHeader(xs: Strings): Try[Header] =
    val wsys: Seq[Try[Strings]] = for (x <- xs.tail) yield parser.parseRow(x, -1)
    for (w <- Try(xs.head); ws <- parser.parseRow((w, -1)); wss <- FP.sequence(wsys)) yield Header(ws, wss)

  private def doConversion(indexedString: (String, Int), header: Header, ws: Strings) =
    RowValues(Row(ws, header, indexedString._2)).convertTo[Row]

object StandardRowParser:
  def create[Row: CellParser](using rowConfig: RowConfig): StandardRowParser[Row] = StandardRowParser(LineParser.apply)

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
case class StandardStringsParser[Row: CellParser]() extends StringsParser[Row]:

  /**
   * Parses a given indexed String (row data and its index) using the specified table header to produce a Try[Row].
   *
   * @param header        the table Header object providing metadata about the row's structure (e.g., column names).
   * @param indexedString a tuple where the first element is the sequence of Strings representing the row data,
   *                      and the second element is the index of the row in the table.
   * @return a Try[Row], which is a Row object if parsing is successful, or a failure if parsing encounters an error.
   */
  def parseIndexed(header: Header)(indexedString: (Strings, Int)): Try[Row] =
    RowValues(Row(indexedString._1, header, indexedString._2)).convertTo[Row]

  /**
   * Parses a given indexed sequence of Strings (as parseIndexed does) unless they fail the raw predicate,
   * which is applied before they are converted.
   *
   * @param header            the table Header object providing metadata about the row's structure (e.g., column names).
   * @param maybeRawPredicate an optional predicate on the row's raw cells (None: parse every row).
   * @param indexedString     a tuple of the row's Strings and its index in the table.
   * @return a Try of Some(row) if the row was parsed, or None if it failed the raw predicate.
   */
  override def parseIndexedIf(header: Header, maybeRawPredicate: Option[RawRow => Boolean])(indexedString: (Strings, Int)): Try[Option[Row]] =
    RowParser.convertIf(maybeRawPredicate, header, indexedString._1)(parseIndexed(header)(indexedString))

  /**
   * Parses the given input using the provided header and produces a result wrapped in a Try.
   *
   * @param header the header object that defines the schema or metadata for parsing the row.
   * @param input  the input of type Input to be parsed into a row of type Row.
   * @return a Try[Row], where Success contains the parsed row if parsing succeeds,
   *         or Failure contains an exception if parsing fails.
   */
  def parse(header: Header)(input: Strings): Try[Row] =
    parseIndexed(header)(input -> 0) // NOTE this is a kluge in that we don't have a line sequence number so we just use 0 instead.

  /**
   * Method to parse a sequence of Strings as a Try[Header].
   *
   * @param ws the header row as a sequence of Strings.
   * @return a Try[Header].
   */
  def parseHeader(ws: Seq[Strings]): Try[Header] = Try(Header(ws))
