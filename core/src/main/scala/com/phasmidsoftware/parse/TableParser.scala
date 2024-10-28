/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import cats.effect.IO
import com.phasmidsoftware.crypto.HexEncryption
import com.phasmidsoftware.parse.AbstractTableParser.logException
import com.phasmidsoftware.parse.TableParser.includeAll
import com.phasmidsoftware.table._
import com.phasmidsoftware.util.FP.{partition, sequence}
import com.phasmidsoftware.util._
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
   * The input type.
   */
  type Input

  /**
   * This variable determines if there is a programmed, i.e. fixed, header for the parser.
   * If its value is None, it signifies that we must look to the first line of data
   * for an appropriate header.
   */
  protected val maybeFixedHeader: Option[Header]

  /**
   * This indicates the number of header rows which must be read from the input.
   * If maybeFixedHeader exists, then this number should be zero.
   */
  val headerRowsToRead: Int

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
  protected def builder(rows: Iterable[Row], header: Header): Table

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
  protected val rowParser: RowParser[Row, Input]

  /**
   * Method to parse a table based on a sequence of Inputs.
   *
   * @param xs the sequence of Inputs, one for each row
   * @return an IO[Table]
   */
  def parse(xs: Iterator[Input], n: Int): IO[Table]
}

object TableParser {

  /**
   * Class to allow the simplification of an expression to parse a source, given a StringTableParser.
   *
   * @param p a StringTableParser.
   * @tparam T the underlying type of p (T will be Table[_]).
   */
  implicit class ImplicitParser[T](p: StringTableParser[T]) {

    /**
     * Method to parse a IO[Source].
     * NOTE the underlying source of sy will be closed after parsing has been completed (no resource leaks).
     *
     * @param si a IO[Source].
     * @return an IO[T].
     */
    def parse(si: IO[Source]): IO[T] = si flatMap doParse

    /**
     * Method to parse an iterator of String.
     *
     * @param xs an Iterator[String].
     * @return an IO[T].
     */
    private def doParse(xs: Iterator[String]): IO[T] = p.parse(xs, 1)

    /**
     * Method to parse a Source.
     * NOTE the source s will be closed after parsing has been completed (no resource leaks).
     *
     * @param s a Source.
     * @return an IO[T].
     */
    private def doParse(s: Source): IO[T] = IOUsing(s)(x => doParse(x.getLines()))
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
    case Success(_) => r.nextInt(n) == 0
    case _ => false
  }

  /**
   * a function which always evaluates as true, regardless of the successfulness of the input.
   */
  val includeAll: Try[Any] => Boolean = _ => true
}

trait CopyableTableParser[Row, Input, Table] {
  def setHeader(header: Header): TableParser[Table]

  def setForgiving(forgiving: Boolean): TableParser[Table]

  def setMultiline(multiline: Boolean): TableParser[Table]

  def setPredicate(predicate: Try[Row] => Boolean): TableParser[Table]

  def setRowParser(rowParser: RowParser[Row, Input]): TableParser[Table]
}

/**
 * Class used to parse files as a Table of Seq[String].
 * That's to say, no parsing of individual (or groups of) columns.
 *
 * @param predicate        a predicate which, if true, allows inclusion of the input row.
 * @param maybeFixedHeader an optional fixed header. If None, we expect to find the header defined in the first line of the file.
 * @param forgiving        forcing (defaults to true). If true then an individual malformed row will not prevent subsequent rows being parsed.
 */
case class RawTableParser(override protected val predicate: Try[RawRow] => Boolean = TableParser.includeAll, maybeFixedHeader: Option[Header] = None, override val forgiving: Boolean = false, override val multiline: Boolean = false, override val headerRowsToRead: Int = 1)
        extends StringTableParser[RawTable] with CopyableTableParser[RawRow, String, RawTable] {

  type Row = RawRow

  implicit val stringSeqParser: CellParser[Strings] = StdCellParsers.cellParserSeq
  implicit val rowCellParser: CellParser[RawRow] = StdCellParsers.rawRowCellParser


  val rowParser: RowParser[Row, String] = StandardRowParser.create[Row]

  // CONSIDER why do we have a concrete Table type mentioned here?
  protected def builder(rows: Iterable[Row], header: Header): Table[Row] = HeadedTable(Content(rows), header)

  def setHeader(header: Header): RawTableParser = copy(maybeFixedHeader = Some(header))

  def setForgiving(b: Boolean): RawTableParser = copy(forgiving = b)

  def setMultiline(b: Boolean): RawTableParser = copy(multiline = b)

  def setPredicate(p: Try[Row] => Boolean): RawTableParser = copy(predicate = p)

  def setRowParser(rp: RowParser[Row, String]): RawTableParser = new RawTableParser(predicate, maybeFixedHeader, forgiving, multiline) {
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
case class PlainTextHeadedStringTableParser[X: CellParser : ClassTag](maybeFixedHeader: Option[Header] = None, override val forgiving: Boolean = false, override val headerRowsToRead: Int = 1)
        extends HeadedStringTableParser[X](maybeFixedHeader, forgiving, headerRowsToRead) {

  def setHeader(header: Header): PlainTextHeadedStringTableParser[X] = copy(maybeFixedHeader = Some(header))

  def setForgiving(b: Boolean): PlainTextHeadedStringTableParser[X] = copy(forgiving = b)

  def setMultiline(b: Boolean): PlainTextHeadedStringTableParser[X] = new PlainTextHeadedStringTableParser[X](maybeFixedHeader, forgiving) {
    override val multiline: Boolean = b
  }

  def setPredicate(p: Try[X] => Boolean): PlainTextHeadedStringTableParser[X] = new PlainTextHeadedStringTableParser[X](maybeFixedHeader, forgiving) {
    override val predicate: Try[X] => Boolean = p
  }

  def setRowParser(rp: RowParser[X, Input]): TableParser[Table[X]] = new PlainTextHeadedStringTableParser[X] {
    override protected val rowParser: RowParser[X, String] = rp
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
 * @param encryptedRowPredicate a function which takes a String and returns a Boolean.
 * @param keyFunction           a function which takes a String and returns a String (input might be ignored).
 * @param maybeFixedHeader      None => requires that the data source has a header row.
 *                              Some(h) => specifies that the header is to be taken from h.
 *                              Defaults to None.
 *                              NOTE: that the simplest is to specify the header directly from the type X.
 * @param forgiving             if true, exceptions when parsing individual rows will be logged then ignored.
 *                              if false, any exception will terminate the parsing.
 *                              Defaults to false.
 * @param headerRowsToRead      the number of header rows expected in the input file
 *                              defaults to 1.
 * @tparam A the cipher algorithm (for which there must be evidence of HexEncryption[A]).
 * @tparam X the underlying row type for which there must be evidence of a CellParser and ClassTag.
 */
case class EncryptedHeadedStringTableParser[X: CellParser : ClassTag, A: HexEncryption](encryptedRowPredicate: String => Boolean, keyFunction: String => String, maybeFixedHeader: Option[Header] = None, override val forgiving: Boolean = false, override val headerRowsToRead: Int = 1)
        extends HeadedStringTableParser[X](None, false, headerRowsToRead) {

  private val phase2Parser = PlainTextHeadedStringTableParser(None, forgiving, headerRowsToRead)

  /**
   * TESTME
   *
   * @param xr the sequence of Inputs, one for each row
   * @param n  the number of lines that should be used as a Header.
   *           If n == 0 == maybeFixedHeader.empty then there is a logic error.
   * @return an IO[Table]
   */
  override def parse(xr: Iterator[String], n: Int): IO[Table[X]] = {
    def decryptAndParse(h: Header, xt: RawTable): IO[Table[X]] = for (wt <- decryptTable(xt); xt <- phase2Parser.parseRows(wt.iterator, h)) yield xt

    val sr: TeeIterator[String] = new TeeIterator(n)(xr)
    val hi: IO[Header] = rowParser.parseHeader(sr.tee)
    val xti: IO[RawTable] = createPhase1Parser.parse(sr)
    for (h <- hi; xt1 <- xti; xt2 <- decryptAndParse(h, xt1)) yield xt2
  }

  /**
   * Set the Header for the plaintext TableParser.
   *
   * CONSIDER does this make sense to allow?
   *
   * @param header the required Header.
   * @return a TableParser of Table[X]
   */
  def setHeader(header: Header): TableParser[Table[X]] =
    throw TableParserException("it makes no sense to allow setting the header of the plaintext parser independently of the encrypted parser")

  /**
   * Set the predicate for the plaintext TableParser.
   *
   * @param predicate a predicate which will be applied to each X (i.e. AFTER decryption).
   * @return a TableParser of Table[X]
   */
  def setPredicate(predicate: Try[X] => Boolean): TableParser[Table[X]] = phase2Parser.setPredicate(predicate)

  /**
   * Set the value of forgiving for the plaintext TableParser.
   *
   * @param b true or false. See TableParser.
   * @return a TableParser of Table[X]
   */
  def setForgiving(b: Boolean): TableParser[Table[X]] = phase2Parser.setForgiving(b)

  /**
   * Set the value of multiline for the plaintext TableParser.
   *
   * @param b value of multiline for the plaintext TableParser. See TableParser.
   * @return a TableParser of Table[X]
   */
  def setMultiline(b: Boolean): TableParser[Table[X]] = phase2Parser.setMultiline(b)

  /**
   * Set the value of predicate for the plaintext TableParser.
   *
   * @param p predicate for the plaintext TableParser.
   * @return a TableParser of Table[X]
   */
  def setPlaintextPredicate(p: Try[X] => Boolean): TableParser[Table[X]] = phase2Parser.setPredicate(p)

  /**
   * Set the value of the row parser for the plaintext TableParser.
   *
   * @param rp the row parser for the plaintext TableParser.
   * @return a TableParser of Table[X]
   */
  def setRowParser(rp: RowParser[X, Input]): TableParser[Table[X]] = phase2Parser.setRowParser(rp)

  private def createPhase1Parser = {
    def rawPredicate(ry: Try[RawRow]): Boolean = ry.map(r => encryptedRowPredicate(r.ws.head)).toOption.getOrElse(false)

    val encryptionHeader: Header = Header(Seq("key", "value"), Nil)
    val rowConfig = RowConfig.defaultEncryptedRowConfig
    implicit val rawRowCellParser: CellParser[RawRow] = StdCellParsers.rawRowCellParser
    val lineParser: LineParser = LineParser.apply(rowConfig)
    RawTableParser(rawPredicate, Some(encryptionHeader), forgiving = false, multiline = false, headerRowsToRead).setRowParser(StandardRowParser[RawRow](lineParser))
  }

  import cats.effect.IO

  private def decryptTable(xt: RawTable): IO[Table[String]] = {
    val wit: Table[IO[String]] = xt.map(row => HexEncryption.decryptRow(keyFunction)(row.ws))
    for (ws <- IO.parSequenceN(2)(wit.toSeq)) yield wit.unit(ws)
  }
}

/**
 * Abstract class to define a StringTableParser that assumes a header to be found in the input file.
 * There are two sub-classes: PlainTextHeadedStringTableParser and EncryptedHeadedStringTableParser
 * This class attempts to provide as much built-in functionality as possible.
 *
 * This class assumes that the names of the columns are in the first line.
 * This class implements builder with a HeadedTable object.
 * This class uses StandardRowParser of its rowParser.
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
sealed abstract class HeadedStringTableParser[X: CellParser : ClassTag](maybeFixedHeader: Option[Header] = None, override val forgiving: Boolean = false, override val headerRowsToRead: Int = 1)
        extends StringTableParser[Table[X]] with CopyableTableParser[X, String, Table[X]] {

  type Row = X

  protected def builder(rows: Iterable[X], header: Header): Table[Row] = HeadedTable(Content(rows), header)

  protected val rowParser: RowParser[X, String] = StandardRowParser.create[X]
}

object HeadedStringTableParser {
  /**
   * This create method constructs a HeadedStringTableParser with header based simply on the type X.
   * In this case, the source data must have the same number of columns as X has parameters, and they must be in the
   * same order. Additionally, there should not be a header row in the source data.
   *
   * @tparam X the underlying type. There must be evidence of CellParser[X] and ClassTag[X].
   * @return a HeadedStringTableParser[X].
   */
  def create[X: CellParser : ClassTag](forgiving: Boolean): HeadedStringTableParser[X] = PlainTextHeadedStringTableParser[X](Some(Header.apply[X]()), forgiving, 0)
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
   * @return an IO[Table]
   */
  def parseRows(xs: Iterator[Input], header: Header): IO[Table]

  /**
   * Method to parse a table based on a sequence of Inputs.
   *
   * NOTE: this is invoked implicitly by:
   * def parse[T: TableParser](ws: Iterator[String]): IO[T]
   * in Table object.
   *
   * @param xr the sequence of Inputs, one for each row
   * @param n  the number of lines that should be used as a Header.
   *           If n == 0 == maybeFixedHeader.empty then there is a logic error.
   * @return an IO[Table]
   */
  def parse(xr: Iterator[Input], n: Int = 0): IO[Table] = maybeFixedHeader match {
    case Some(h) => parseRows(xr drop n, h) // CONSIDER reverting to check that n = 0
    case None if n > 0 =>
      val yr: TeeIterator[Input] = new TeeIterator(n)(xr)
      for (h <- rowParser.parseHeader(yr.tee); t <- parseRows(yr, h)) yield t
    case _ => IO.raiseError(TableParserException("parse: logic error"))
  }

  /**
   * Common code for parsing rows.
   *
   * CONSIDER convert T to Input
   *
   * CONSIDER switch order of f
   *
   * @param ts     a sequence of Ts.
   * @param header the Header.
   * @param f      a curried function which transforms a (T, Int) into a function which is of type Header => Try[Row].
   * @tparam T the parametric type of the resulting Table. T corresponds to Input in the calling method, i.e. a Row. Must be Joinable.
   * @return a Try of Table
   */
  protected def doParseRows[T: Joinable](ts: Iterator[T], header: Header, f: ((T, Int)) => Header => Try[Row]): Try[Table] = {
    implicit object Z extends Joinable[(T, Int)] {
      private val tj: Joinable[T] = implicitly[Joinable[T]]

      def join(t1: (T, Int), t2: (T, Int)): (T, Int) = tj.join(t1._1, t2._1) -> (if (t1._2 >= 0) t1._2 else t2._2)

      val zero: (T, Int) = tj.zero -> -1

      def valid(t: (T, Int)): Boolean = tj.valid(t._1)
    }

    def mapTsToRows = if (multiline)
      for (z <- new FunctionIterator[(T, Int), Row](f(_)(header))(ts.zipWithIndex)) yield z
    else
      for (z <- ts.zipWithIndex) yield f(z)(header)

    def processTriedRows(rys: Iterator[Try[Row]]) = if (forgiving) {
      val (good, bad) = partition(rys)
      // CONSIDER using sequenceRev in order to save time
      bad foreach failureHandler //AbstractTableParser.logException[Row]
      sequence(good filter predicate)
    }
    else
      sequence(rys filter predicate)

    val q: Seq[Try[Row]] = mapTsToRows.toSeq
    for (rs <- processTriedRows(q.iterator)) yield builder(rs.toList, header)
  }

  private def failureHandler(ry: Try[Row]): Unit = logException[Row](ry)
}

object AbstractTableParser {
  def logException(e: Throwable): Unit = {
    val string = s"${e.getLocalizedMessage}${
      if (e.getCause == null) "" else s" caused by ${e.getCause.getLocalizedMessage}"
    }"
    TableParser.logger.warn(string)
  }

  def logException[X](xy: Try[X]): Unit = xy match {
    case Success(_) =>
    case Failure(exception) => logException(exception)
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

  def parseRows(wr: Iterator[String], header: Header): IO[Table] = IO.fromTry(doParseRows(wr, header, rowParser.parse))
}

/**
 * Abstract class to extend AbstractTableParser but with Input = Strings
 * This is the unusual situation where a file is a sequence of a sequence of Strings, each representing one value.
 *
 * @tparam Table the table type.
 */
abstract class StringsTableParser[Table] extends AbstractTableParser[Table] {
  type Input = Strings

  def parseRows(wsr: Iterator[Strings], header: Header): IO[Table] = IO.fromTry(doParseRows(wsr, header, rowParser.parse))
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

  implicit val ptp: TableParser[Table[X]] = if (sourceHasHeaderRow) PlainTextHeadedStringTableParser[X](None, forgiving) else HeadedStringTableParser.create[X](forgiving)
}

// NOTE: not currently instantiated
case class TableParserException(msg: String, e: Option[Throwable] = None) extends Exception(msg, e.orNull)
