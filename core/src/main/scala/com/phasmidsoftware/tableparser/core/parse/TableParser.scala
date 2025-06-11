/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.parse.TableParser.includeAll
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.util.{Joinable, TeeIterator, TryUsing}
import org.slf4j.{Logger, LoggerFactory}
import scala.annotation.implicitNotFound
import scala.io.Source
import scala.reflect.ClassTag
import scala.util.{Failure, Random, Success, Try}

/**
 * Type class to parse a set of rows as a Table.
 *
 * @tparam Table the Table type.
 */
@implicitNotFound(msg = "Cannot find an implicit instance of TableParser[${Table}]. Typically, you should define an instance of StringTableParser or StringsTableParser.")
trait TableParser[Table] {

  /**
   * The row type.
   */
  type Row

  /**
   * The input type, typically `String` or `Strings`.
   */
  type Input

  /**
   * This variable determines if there is a programmed, i.e., fixed, header for the parser.
   * If its value is None, it signifies that we must look to the first line(s) of data
   * for an appropriate header.
   */
  protected val maybeHeader: Option[Header] = None

  /**
   * This indicates the number of header rows which must be read from the input.
   * The value is usually 1 but some types of CSV file have more than one header line.
   * If maybeHeader exists, then this number is ignored.
   *
   * CONSIDER if this is really ignored when maybeHeader is defined, then we should eliminate such places where this value has an override of 0.
   */
  val headerRowsToRead: Int = 1

  /**
   * Method to construct a Table based on the provided rows and header.
   *
   * @param rows   an iterator of Row objects representing the data rows.
   * @param header a Header object representing the table's column headers.
   * @return the constructed Table based on the input rows and header.
   */
  protected def builder(rows: Iterator[Row], header: Header): Table

  /**
   * Method to determine how errors are handled.
   *
   * @return true if individual errors are logged but do not cause parsing to fail.
   */
  protected val forgiving: Boolean = false

  /**
   * Value to determine whether it is acceptable to have a quoted string span more than one line.
   *
   * @return true if quoted strings may span more than one line.
   */
  protected val multiline: Boolean = false

  /**
   * Function to determine whether or not a row should be included in the table.
   * Typically used for random sampling.
   */
  protected val predicate: Try[Row] => Boolean = includeAll

  /**
   * Method to define a row parser.
   *
   * @return a RowParser[Row, Input].
   */
  val rowParser: RowParser[Row, Input]

  /**
   * Method to parse a table based on a sequence of Inputs.
   *
   * @param xs the sequence of Inputs, one for each row
   * @param n  the number of rows to drop (length of the header).
   * @return an Try[Table]
   */
  def parse(xs: Iterator[Input], n: Int = headerRowsToRead): Try[Table]
}

/**
 * The `TableParser` object provides functionality for parsing tabular data from various sources
 * using a `StringTableParser`. It includes utilities for character-by-character parsing of sources,
 * working with sampled data, and handling parsing results effectively. The object also provides
 * implicit and utility functions to enhance parsing workflows.
 */
object TableParser {

  /**
   * Class to allow the simplification of an expression to parse a source, given a `StringTableParser`.
   *
   * @param p a StringTableParser.
   * @tparam Table the (parametric) underlying type of `p` (`Table` will be `Table[_]`).
   */
  implicit class ImplicitParser[Table](p: StringTableParser[Table]) {

    /**
     * Method to parse a `Try[Source]` character-by-character.
     * NOTE the underlying source of `sy` will be closed after parsing has been completed.
     *
     * @param sy a `Try[Source]`.
     * @return a `Try[Table]`.
     */
    def parse(sy: Try[Source]): Try[Table] =
      sy flatMap doParse

    /**
     * Method to parse a `Source`.
     * NOTE the source `s` will be closed after parsing has been completed (no resource leaks).
     *
     * @param s a Source.
     * @return a `Try[Table]`.
     */
    private def doParse(s: Source): Try[Table] =
      TryUsing(s)(x => doParse(x.getLines()))

    /**
     * Method to parse an iterator of String.
     *
     * @param xs an `Iterator[String]`.
     * @return a `Try[Table]`.
     */
    private def doParse(xs: Iterator[String]): Try[Table] =
      p.parse(xs, 1)
  }

  val r: Random = new Random()

  val logger: Logger = LoggerFactory.getLogger(TableParser.getClass)

  /**
   * Method to return a random sampling function.
   *
   * CONSIDER using FP.sampler
   *
   * @param n this is the sample factor: approximately one in every n successful results will form part of the result.
   * @tparam X the underlying type of the sampler.
   * @return a Try[X] => Boolean function which is always yields false if its input is a failure, otherwise,
   *         it chooses every nth value (approximately).
   */
  def sampler[X](n: Int): Try[X] => Boolean = {
    case Success(_) =>
      r.nextInt(n) == 0
    case _ =>
      false
  }

  /**
   * a function which always evaluates as true, regardless of the successfulness of the input.
   */
  val includeAll: Try[Any] => Boolean = _ => true
}

/**
 * A trait representing a parser for tables that supports configurability and row-level parsing.
 *
 * The `CopyableTableParser` provides methods to configure the parsing behavior of tables, such
 * as defining headers, enabling forgiving or multiline parsing modes, setting predicates for
 * row filtering, and defining custom row parsing logic. It operates generically over three
 * types: `Row`, representing a single row in the table; `Input`, representing raw input data for
 * parsing rows; and `Table`, representing the parsed table as a whole.
 *
 * @tparam Row   the type of a single row in the table.
 * @tparam Input the type of the raw input data used to parse rows.
 * @tparam Table the type of the parsed table.
 */
trait CopyableTableParser[Row, Input, Table] {
  /**
   * Sets a fixed header for the table parser.
   * This method allows the configuration of the parser to use the specified header,
   * instead of deriving it from the data during parsing.
   *
   * @param header the `Header` instance representing the fixed header for the table parser,
   *               containing column names and optional additional header lines.
   * @return a new `TableParser[Table]` instance configured with the specified header.
   */
  def setHeader(header: Header): TableParser[Table]

  /**
   * Configures the parser to operate in a "forgiving" mode, where errors encountered during the
   * parsing of individual rows are logged but do not cause the parsing process to fail. When set
   * to `true`, the parser will attempt to continue parsing despite encountering errors in the input.
   * When set to `false`, errors will cause the parsing process to terminate.
   *
   * @param forgiving a boolean value indicating whether forgiving mode should be enabled.
   *                  `true` enables forgiving mode, allowing parsing to continue on errors.
   *                  `false` disables forgiving mode, causing failures on errors.
   * @return an instance of `TableParser[Table]` configured with the specified forgiving behavior.
   */
  def setForgiving(forgiving: Boolean): TableParser[Table]

  /**
   * Configures the parser to allow or disallow quoted strings to span across multiple lines.
   *
   * When `multiline` is set to `true`, the parser permits quoted strings in the input to extend
   * beyond a single line. If set to `false`, quoted strings are constrained to stay within one line.
   *
   * @param multiline a boolean flag indicating whether multiline parsing of quoted strings is enabled (`true`)
   *                  or disabled (`false`).
   * @return a new `TableParser[Table]` instance configured with the updated multiline setting.
   */
  def setMultiline(multiline: Boolean): TableParser[Table]

  /**
   * Sets a predicate function that determines whether a parsed `Row` should be included in the final `Table`.
   * The predicate is applied to each parsed `Row` wrapped in a `Try` to allow filtering based on success or failure.
   *
   * @param predicate a function of type `Try[Row] => Boolean` that accepts a `Try` of a `Row` and returns `true`
   *                  if the row should be included in the table, or `false` otherwise.
   * @return a new instance of `TableParser[Table]` with the specified predicate applied.
   */
  def setPredicate(predicate: Try[Row] => Boolean): TableParser[Table]

  /**
   * Sets the row parser to be used for parsing rows in the table.
   *
   * The row parser provides the logic for converting raw input data of type `Input`
   * into rows of type `Row`. This method allows you to configure the table parser
   * with a custom row parser, enabling flexibility in interpreting the rows
   * according to specific requirements.
   *
   * @param rowParser the `RowParser` instance to use for parsing rows. The `RowParser`
   *                  defines how each input (of type `Input`) is transformed into a
   *                  row (of type `Row`), potentially using a `Header` for additional context.
   * @return a new `TableParser` instance configured with the specified row parser.
   */
  def setRowParser(rowParser: RowParser[Row, Input]): TableParser[Table]
}

/**
 * Class used to parse files as a Table of Seq[String].
 * That's to say, no parsing of individual (or groups of) columns.
 *
 * @param predicate        a predicate which, if true, allows inclusion of the input row.
 * @param maybeHeader      an optional fixed header. If None, we expect to find the header defined in the first line of the file.
 * @param forgiving        forcing (defaults to true). If true then an individual malformed row will not prevent subsequent rows being parsed.
 */
case class RawTableParser(override protected val predicate: Try[RawRow] => Boolean = TableParser.includeAll, override val maybeHeader: Option[Header] = None, override val forgiving: Boolean = false, override val multiline: Boolean = false, override val headerRowsToRead: Int = 1)
        extends StringTableParser[RawTable] with CopyableTableParser[RawRow, String, RawTable] {

  type Row = RawRow

  implicit val stringSeqParser: CellParser[Strings] = StdCellParsers.cellParserSeq
  implicit val rowCellParser: CellParser[RawRow] = StdCellParsers.rawRowCellParser

  val rowParser: RowParser[Row, String] = StandardRowParser.create[Row]

  // CONSIDER why do we have a concrete Table type mentioned here?
  protected def builder(rows: Iterator[RawRow], header: Header): Table[Row] =
    HeadedTable(Content(rows), header)

  def setHeader(header: Header): RawTableParser =
    copy(maybeHeader = Some(header))

  def setForgiving(b: Boolean): RawTableParser =
    copy(forgiving = b)

  def setMultiline(b: Boolean): RawTableParser =
    copy(multiline = b)

  def setPredicate(p: Try[Row] => Boolean): RawTableParser =
    copy(predicate = p)

  def setRowParser(rp: RowParser[Row, String]): RawTableParser =
    new RawTableParser(predicate, maybeHeader, forgiving, multiline) {
      override val rowParser: RowParser[Row, String] = rp
  }
}

/**
 * Case class to define a StringTableParser that assumes a header to be found in the input file.
 * This class attempts to provide as much built-in functionality as possible.
 *
 * This class assumes that the names of the columns are in the first line.
 * This class implements builder with a HeadedTable object.
 * This class uses StandardRowParser of its rowParser.
 *
 * @param maybeHeader      None => requires that the data source has a header row.
 *                         Some(h) => specifies that the header is to be taken from h.
 *                         Defaults to None.
 *                         NOTE: that the simplest is to specify the header directly from the type X.
 * @param forgiving        if true, exceptions when parsing individual rows will be logged then ignored.
 *                         if false, any exception will terminate the parsing.
 *                         Defaults to false.
 * @param headerRowsToRead the number of header rows expected in the input file
 *                         defaults to 1.
 * @see HeadedStringTableParser#create
 * @tparam X the underlying row type for which there must be evidence of a CellParser and ClassTag.
 */
case class PlainTextHeadedStringTableParser[X: CellParser : ClassTag](override val maybeHeader: Option[Header] = None, override val forgiving: Boolean = false, override val headerRowsToRead: Int = 1)
        extends HeadedStringTableParser[X](maybeHeader, forgiving, headerRowsToRead) {

  def setHeader(header: Header): PlainTextHeadedStringTableParser[X] =
    copy(maybeHeader = Some(header))

  def setForgiving(b: Boolean): PlainTextHeadedStringTableParser[X] =
    copy(forgiving = b)

  def setMultiline(b: Boolean): PlainTextHeadedStringTableParser[X] =
    new PlainTextHeadedStringTableParser[X](maybeHeader, forgiving) {
      override val multiline: Boolean = b
  }

  def setPredicate(p: Try[X] => Boolean): PlainTextHeadedStringTableParser[X] =
    new PlainTextHeadedStringTableParser[X](maybeHeader, forgiving) {
    override val predicate: Try[X] => Boolean = p
  }

  def setRowParser(rp: RowParser[X, Input]): TableParser[Table[X]] =
    new PlainTextHeadedStringTableParser[X] {
      override val rowParser: RowParser[X, String] = rp
  }
}

/**
 * Abstract class to define a StringTableParser that assumes a header to be found in the input file.
 * There are two subclasses: PlainTextHeadedStringTableParser and EncryptedHeadedStringTableParser
 * This class attempts to provide as much built-in functionality as possible.
 *
 * This class assumes that the names of the columns are in the first line.
 * This class implements builder with a HeadedTable object.
 * This class uses StandardRowParser of its rowParser.
 *
 * NOTE that this class and HeadedCSVTableParser are extremely similar (maybe identical)
 * CONSIDER merging with HeadedCSVTableParser
 *
 * @param maybeFixedHeader None => requires that the data source has a header row.
 *                         Some(h) => specifies that the header is to be taken from h.
 *                         Defaults to None.
 *                         NOTE: that the simplest is to specify the header directly from the type X.
 * @param forgiving        if true, exceptions when parsing individual rows will be logged then ignored.
 *                         if false, any exception will terminate the parsing.
 *                         Defaults to false.
 * @param headerRowsToRead the number of header rows expected in the input file
 *                         defaults to 1.
 * @see HeadedStringTableParser#create
 * @tparam X the underlying row type for which there must be evidence of a CellParser and ClassTag.
 */
abstract class HeadedStringTableParser[X: CellParser : ClassTag](maybeFixedHeader: Option[Header] = None, override val forgiving: Boolean = false, override val headerRowsToRead: Int = 1)
        extends StringTableParser[Table[X]] with CopyableTableParser[X, String, Table[X]] {

  type Row = X

  protected def builder(rows: Iterator[X], header: Header): Table[Row] =
    HeadedTable(Content(rows), header)

  val rowParser: RowParser[X, String] =
    StandardRowParser.create[X]
}

object HeadedStringTableParser {
  /**
   * This `create` method constructs a HeadedStringTableParser with header based simply on the type X.
   * In this case, the source data must have the same number of columns as X has parameters, and they must be in the
   * same order. Additionally, there should not be a header row in the source data.
   *
   * @tparam X the underlying type. There must be evidence of CellParser[X] and ClassTag[X].
   * @return a HeadedStringTableParser[X].
   */
  def create[X: CellParser : ClassTag](forgiving: Boolean): HeadedStringTableParser[X] =
    PlainTextHeadedStringTableParser[X](Some(Header.apply[X]()), forgiving, 0)
}

/**
 * Abstract base class for implementations of TableParser[T].
 * NOTE: that Table is a parametric type and does NOT refer to the type Table defined elsewhere.
 *
 * CONSIDER making this a trait
 *
 * @tparam Table the (parametric) Table type.
 */
abstract class AbstractTableParser[Table] extends TableParser[Table] {

  /**
   * Abstract method to parse a sequence of Inputs, with a given header.
   *
   * @param xs     the sequence of Inputs, one for each row
   * @param header the header to be used.
   * @return an Try[Table]
   */
  def parseRows(xs: Iterator[Input], header: Header): Try[Table]

  /**
   * Method to parse a table based on a sequence of Inputs.
   *
   * @param xs the sequence of Inputs, one for each row
   * @param n  the number of rows to drop (length of the header).
   * @return a Try[Table].
   */
  def parse(xs: Iterator[Input], n: Int = headerRowsToRead): Try[Table] = maybeHeader match {
    case Some(h) =>
      // CONSIDER eliminating the drop here but there is one test in Cats Tests that will fail
      parseRows(xs drop n, h)
    case None if n > 0 =>
      val yr: TeeIterator[Input] = new TeeIterator(n)(xs)
      for (h <- rowParser.parseHeader(yr.tee); t <- parseRows(yr, h)) yield t
    case _ =>
      Failure(TableParserException("parse: logic error"))
  }

  /**
   * Parses rows of input data into a table representation.
   *
   * This method utilizes an `IndexedInputToRowsAggregator` to transform an iterator of inputs into
   * rows using the provided header and transformation function. It processes the input, materializes
   * the resulting rows into a list, and constructs a table representation from the processed rows and header.
   *
   * @param ts     an iterator of inputs to be parsed into rows.
   * @param header the `Header` containing column metadata to guide the transformation process.
   * @param f      a transformation function that maps the `Header` to a function capable of
   *               transforming a tuple of input and its index into a `Try[Row]`. This function
   *               encapsulates row processing logic and error handling for each input element.
   * @param ev     an implicit evidence parameter ensuring the input type adheres to the `Joinable` type class,
   *               which defines how input elements can be combined or validated during processing.
   * @return a `Try` containing a `Table` if the parsing is successful, or an error in case of failure.
   */
  def doParseRows(ts: Iterator[Input], header: Header, f: ((Input, Int)) => Try[Row])(implicit ev: Joinable[Input]): Try[Table] = {

    val inputTransformer = new IndexedInputToRowsAggregator(f, multiline, forgiving, predicate)

    // NOTE that here, although builder takes an Iterator, all builder implementations that currently exist
    // wrap the rows into Content which forces a materialization of the rows into a list of rows.
    for (rs <- inputTransformer.processInput(ts)) yield builder(rs, header)
  }
}

/**
 * The `AbstractTableParser` companion object provides utility methods for logging exceptions during the table parsing process.
 *
 * This object includes methods to log exceptions, whether they are directly provided as a `Throwable` or wrapped inside a `Try` result.
 */
object AbstractTableParser {
  /**
   * Logs an exception by generating a warning message that includes the localized message of the exception
   * and, if present, the message from the exception's cause.
   *
   * @param e the `Throwable` exception to be logged. This might include a cause that provides additional context.
   * @return `Unit`. The method does not produce a result; it logs the warning message using the `TableParser.logger`.
   */
  def logException(e: Throwable): Unit = {
    val string = s"${e.getLocalizedMessage}${
      if (e.getCause == null) "" else s" caused by ${e.getCause.getLocalizedMessage}"
    }"
    TableParser.logger.warn(string)
  }

  /**
   * Logs an exception contained within a `Try` result. If the `Try` is a `Failure`, the enclosed exception is logged.
   * If it is a `Success`, no action is taken.
   *
   * @param xy a `Try` instance, which may either encapsulate a successful computation (`Success`) or an exception (`Failure`).
   * @return `Unit`. This method does not return a value; it is used only for logging purposes.
   */
  def logException[X](xy: Try[X]): Unit = xy match {
    case Success(_) =>
    // XXX do nothing
    case Failure(exception) =>
      logException(exception)
  }
}

/**
 * Abstract class to extend AbstractTableParser but with Input = String.
 * This is the normal situation where a file is a sequence of Strings, each representing one line.
 *
 * @tparam Table the table type.
 */
abstract class StringTableParser[Table] extends AbstractTableParser[Table] {
  type Input = String

  /**
   * Parses rows from an iterator of input strings (`ws`) using the provided header and row parsing logic.
   *
   * This method utilizes the `doParseRows` function to process each row in the input iterator, applying the
   * provided `rowParser.parse` function to convert input strings into logical rows, while using
   * the header to guide parsing and interpretation of the input.
   *
   * @param ws an iterator over the input strings, where each string represents a row in the table.
   * @param header the `Header` object that describes the expected structure of the table (e.g., column names).
   * @return a `Try` of the parsed `Table` object. On success, it contains the resulting table; on failure,
   *         it contains the parsing error.
   */
  def parseRows(ws: Iterator[String], header: Header): Try[Table] =
    doParseRows(ws, header, rowParser.parseIndexed(header))

  /**
   * Method to parse a table based on a sequence of Inputs.
   * NOTE: this is required so that we can invoke this method with an Iterator[String].
   *
   * @param xs the sequence of Inputs, one for each row
   * @param n  the number of rows to drop (length of the header)--defaults to 1.
   * @return a Try[Table].
   */
  override def parse(xs: Iterator[String], n: Int = headerRowsToRead): Try[Table] = super.parse(xs, n)
}

/**
 * Abstract class for parsing tables from CSV-formatted strings, where the table has a defined header.
 * This is the most convenient subtype of `TableParser` for most CSV files.
 *
 * @tparam T the parametric type of rows in the table.
 * @param rp an implicit `RowParser`, which defines the logic for parsing individual rows of type `T` from strings.
 */
abstract class HeadedCSVTableParser[T: ClassTag](implicit rp: RowParser[T, String]) extends StringTableParser[Table[T]] {
  type Row = T

  /**
   * Default method to create a new table.
   * It does this by invoking either builderWithHeader or builderWithoutHeader, as appropriate.
   *
   * CONSIDER changing Iterable back to Iterator as it was at V1.0.13.
   *
   * @param rows   the rows which will make up the table.
   * @param header the Header, derived either from the program or the data.
   * @return an instance of Table.
   */
  protected def builder(rows: Iterator[T], header: Header): Table[T] = HeadedTable(rows, header)

  /**
   * Method to define a row parser.
   *
   * @return a RowParser[Row, Input].
   */
  val rowParser: RowParser[T, String] = implicitly[RowParser[T, String]]
}

/**
 * Abstract class to extend AbstractTableParser but with Input = Strings
 * This is the unusual situation where a file is a sequence of a sequence of Strings, each representing one value.
 *
 * @tparam Table the table type.
 */
abstract class StringsTableParser[Table] extends AbstractTableParser[Table] {
  type Input = Strings

  /**
   * Parses the rows of a table using the given header and returns the parsed table wrapped in a `Try`.
   *
   * This method processes a sequence of rows (each represented as a `Strings`), leveraging the provided header
   * and a row parser to translate these rows into a structured table of type `Table`.
   *
   * @param wss an `Iterator` of `Strings` where each element represents a row (sequence of cell values).
   * @param header the `Header` object containing metadata about the structure of the table, such as column names.
   * @return a `Try[Table]` containing the parsed table on success or an exception on failure.
   */
  def parseRows(wss: Iterator[Strings], header: Header): Try[Table] =
    doParseRows(wss, header, rowParser.parseIndexed(header))

  /**
   * Method to parse a table based on a sequence of Inputs.
   * NOTE: this is required so that we can invoke this method with an Iterator[Strings].
   *
   * @param xs the sequence of Inputs, one for each row
   * @param n  the number of rows to drop (length of the header) (defaults to 1)
   * @return a Try[Table].
   */
  override def parse(xs: Iterator[Strings], n: Int = headerRowsToRead): Try[Table] = super.parse(xs, n)
}

/**
 * TableParserHelper: abstract class to help with defining an implicit TableParser of Table[X].
 * Note that this class extends CellParser[X].
 * It is expected that this should be sub-classed by the object which is the companion object of X.
 * That will make it easiest for the compiler to discover the implicit value of type TableParser of Table[X]
 *
 * NOTE: this class should be used for simple cases where the the data and type X match according to one of options
 * for sourceHasHeaderRow.
 * More complex situations can easily be handled but not using this TableParserHelper class.
 *
 * @param sourceHasHeaderRow true (default) if the data to be read has an explicit header row with column names that match the parameters
 *                           of type X;
 *                           false if there is no header row in the data AND if the data has (unnamed) columns of the same number
 *                           and in the same order as defined by type X.
 * @param forgiving          true if individual rows of the source which do not parse successfully,
 *                           are logged but otherwise do not affect the success of the overall parsing.
 * @tparam X the type for which we require a TableParser[X].
 */
abstract class TableParserHelper[X: ClassTag](sourceHasHeaderRow: Boolean = true, forgiving: Boolean = false) extends CellParsers {

  /**
   * Abstract method which will return a CellParser[X].
   * NOTE that a typical definition will be something like cellParser2(Player.apply) where, in this case, the number
   * is 2 corresponding to the number of parameters in Player.
   *
   * @return
   */
  def cellParser: CellParser[X]

  implicit val xp: CellParser[X] = cellParser

  implicit val ptp: TableParser[Table[X]] =
    if (sourceHasHeaderRow) PlainTextHeadedStringTableParser[X](None, forgiving) else HeadedStringTableParser.create[X](forgiving)
}

// NOTE: not currently instantiated
case class TableParserException(msg: String, e: Option[Throwable] = None) extends Exception(msg, e.orNull)
