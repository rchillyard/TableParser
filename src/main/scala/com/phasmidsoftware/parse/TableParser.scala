/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.RawRow
import com.phasmidsoftware.crypto.Encryption
import com.phasmidsoftware.parse.AbstractTableParser.logException
import com.phasmidsoftware.parse.TableParser.includeAll
import com.phasmidsoftware.table.{HeadedTable, Header, Table}
import com.phasmidsoftware.util.FP.partition
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
   * @return a Try[Table]
   */
  def parse(xs: Iterator[Input], n: Int): Try[Table]
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
     * Method to parse an iterator of String.
     *
     * @param xs an Iterator[String].
     * @return a Try[T].
     */
    def parse(xs: Iterator[String]): Try[T] = p.parse(xs, 1)

    /**
     * Method to parse a Source.
     * NOTE the source s will be closed after parsing has been completed (no resource leaks).
     *
     * @param s a Source.
     * @return a Try[T].
     */
    def parse(s: Source): Try[T] = TryUsing(s)(x => parse(x.getLines()))

    /**
     * Method to parse a Try[Source].
     * NOTE the underlying source of sy will be closed after parsing has been completed (no resource leaks).
     *
     * @param sy a Source.
     * @return a Try[T].
     */
    def parse(sy: Try[Source]): Try[T] = sy flatMap parse
  }

  val r: Random = new Random()

  val logger: Logger = LoggerFactory.getLogger(TableParser.getClass)

  /**
   * Method to return a random sampling function.
   *
   * @param n this is the sample factor: approximately one in every n successful results will form part of the result.
   * @return a Try[Any] => Boolean function which is always yields false if its input is a failure, otherwise,
   *         it chooses every nth value (approximately).
   */
  def sampler(n: Int): Try[Any] => Boolean = {
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
        extends StringTableParser[Table[RawRow]] with CopyableTableParser[RawRow, String, Table[RawRow]] {

  type Row = RawRow

  implicit val stringSeqParser: CellParser[RawRow] = StdCellParsers.cellParserSeq

  val rowParser: RowParser[Row, String] = StandardRowParser.create[RawRow]

  // CONSIDER why do we have a concrete Table type mentioned here?
  protected def builder(rows: Iterable[Row], header: Header): Table[Row] = HeadedTable(rows, header)

  // TEST
  def setHeader(header: Header): RawTableParser = copy(maybeFixedHeader = Some(header))

  // TEST
  def setForgiving(b: Boolean): RawTableParser = copy(forgiving = b)

  def setMultiline(b: Boolean): RawTableParser = copy(multiline = b)

  def setPredicate(p: Try[RawRow] => Boolean): RawTableParser = copy(predicate = p)

  // TEST
  def setRowParser(rp: RowParser[RawRow, String]): RawTableParser = new RawTableParser(predicate, maybeFixedHeader, forgiving, multiline) {
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
  * @tparam X the underlying row type which must provide evidence of a CellParser and ClassTag.
  */
case class PlainTextHeadedStringTableParser[X: CellParser : ClassTag](maybeFixedHeader: Option[Header] = None, override val forgiving: Boolean = false, override val headerRowsToRead: Int = 1)
  extends HeadedStringTableParser[X](maybeFixedHeader, forgiving, headerRowsToRead) {

  // TEST
  def setHeader(header: Header): PlainTextHeadedStringTableParser[X] = copy(maybeFixedHeader = Some(header))

  // TEST
  def setForgiving(b: Boolean): PlainTextHeadedStringTableParser[X] = copy(forgiving = b)

  // TEST
  def setMultiline(b: Boolean): PlainTextHeadedStringTableParser[X] = new PlainTextHeadedStringTableParser[X](maybeFixedHeader, forgiving) {
    override val multiline: Boolean = b
  }

  // TEST
  def setPredicate(p: Try[X] => Boolean): PlainTextHeadedStringTableParser[X] = new PlainTextHeadedStringTableParser[X](maybeFixedHeader, forgiving) {
    override val predicate: Try[X] => Boolean = p
  }

  // TEST
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
  * @param maybeFixedHeader None => requires that the data source has a header row.
 *                          Some(h) => specifies that the header is to be taken from h.
 *                          Defaults to None.
 *                          NOTE: that the simplest is to specify the header directly from the type X.
 * @param forgiving         if true, exceptions when parsing individual rows will be logged then ignored.
 *                          if false, any exception will terminate the parsing.
 *                          Defaults to false.
 * @param headerRowsToRead  the number of header rows expected in the input file
 *                          defaults to 1.
 * @tparam X the underlying row type which must provide evidence of a CellParser and ClassTag.
 */
case class EncryptedHeadedStringTableParser[X: CellParser : ClassTag](encryptedRowPredicate: String => Boolean, keyMap: Map[String, String], maybeFixedHeader: Option[Header] = None, override val forgiving: Boolean = false, override val headerRowsToRead: Int = 1)
        extends HeadedStringTableParser[X](None, false, headerRowsToRead) {

  private val phase2Parser = PlainTextHeadedStringTableParser(None, forgiving, headerRowsToRead)

  override def parse(xs: Iterator[String], n: Int): Try[Table[X]] = {
    val ys = new TeeIterator(n)(xs)
    val hy: Try[Header] = rowParser.parseHeader(ys.tee)
    // Phase 1: read the encrypted rows
    val xty: Try[Table[RawRow]] = createPhase1Parser.parse(ys)
    (for (h <- hy; xt <- xty) yield (h, xt)) match {
      case Success((h, xt)) =>
        // Phase 2: decrypt the rows
        val yt: Table[String] = xt.map(row => Encryption.decrypt(keyMap)(row))
        // Phase 2: parse the plain text rows.
        phase2Parser.parseRows(yt.rows.iterator, h)
    }
  }

  /**
    * Set the Header for the plaintext TableParser.
    *
    * TEST
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
    * TEST
    *
    * @param predicate a predicate which will be applied to each X (i.e. AFTER decryption).
    * @return a TableParser of Table[X]
    */
  def setPredicate(predicate: Try[X] => Boolean): TableParser[Table[X]] = phase2Parser.setPredicate(predicate)

  /**
    * Set the value of forgiving for the plaintext TableParser.
    *
    * TEST
    *
    * @param b true or false. See TableParser.
    * @return a TableParser of Table[X]
    */
  def setForgiving(b: Boolean): TableParser[Table[X]] = phase2Parser.setForgiving(b)

  /**
    * Set the value of multiline for the plaintext TableParser.
    *
    * TEST
    *
    * @param b value of multiline for the plaintext TableParser. See TableParser.
    * @return a TableParser of Table[X]
    */
  def setMultiline(b: Boolean): TableParser[Table[X]] = phase2Parser.setMultiline(b)

  /**
    * Set the value of predicate for the plaintext TableParser.
    *
    * TEST
    *
    * @param p predicate for the plaintext TableParser.
    * @return a TableParser of Table[X]
    */
  def setPlaintextPredicate(p: Try[X] => Boolean): TableParser[Table[X]] = phase2Parser.setPredicate(p)

  /**
    * Set the value of the row parser for the plaintext TableParser.
    *
    * TEST
    *
    * @param rp the row parser for the plaintext TableParser.
    * @return a TableParser of Table[X]
    */
  def setRowParser(rp: RowParser[X, Input]): TableParser[Table[X]] = phase2Parser.setRowParser(rp)

  private def createPhase1Parser = {
    def rawPredicate(r: Try[RawRow]): Boolean = r.map(ws => encryptedRowPredicate(ws.head)).toOption.getOrElse(false)

    val encryptionHeader: Header = Header(Seq("key", "value"), Nil)
    val rowConfig = RowConfig.defaultEncryptedRowConfig
    import RawParsers.WithHeaderRow.rawRowCellParser
    val lineParser: LineParser = LineParser.apply(rowConfig)
    RawTableParser(rawPredicate, Some(encryptionHeader), forgiving = false, multiline = false, headerRowsToRead).setRowParser(StandardRowParser[RawRow](lineParser))
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
  * @tparam X the underlying row type which must provide evidence of a CellParser and ClassTag.
  */
sealed abstract class HeadedStringTableParser[X: CellParser : ClassTag](maybeFixedHeader: Option[Header] = None, override val forgiving: Boolean = false, override val headerRowsToRead: Int = 1)
  extends StringTableParser[Table[X]] with CopyableTableParser[X, String, Table[X]] {

  type Row = X

  protected def builder(rows: Iterable[X], header: Header): Table[Row] = maybeFixedHeader match {
    case Some(h) => HeadedTable(rows, h)
    case None => HeadedTable(rows, Header[Row]()) // CHECK
  }

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
 * @tparam Table the (parametric) Table type.
 */
abstract class AbstractTableParser[Table] extends TableParser[Table] {

  protected def failureHandler(ry: Try[Row]): Unit = logException[Row](ry)

  /**
   * Abstract method to parse a sequence of Inputs, with a given header.
   *
   * @param xs     the sequence of Inputs, one for each row
   * @param header the header to be used.
   * @return a Try[Table]
   */
  def parseRows(xs: Iterator[Input], header: Header): Try[Table]

  /**
    * Method to parse a table based on a sequence of Inputs.
    *
    * NOTE: this is invoked implicitly by:
    * def parse[T: TableParser](ws: Iterator[String]): Try[T]
    * in Table object.
    *
    * @param xs the sequence of Inputs, one for each row
    * @param n  the number of lines that should be used as a Header.
    *           If n == 0 == maybeFixedHeader.empty then there is a logic error.
    * @return a Try[Table]
    */
  def parse(xs: Iterator[Input], n: Int = 0): Try[Table] = maybeFixedHeader match {
    case Some(h) => parseRows(xs drop n, h) // CONSIDER reverting to check that n = 0
    case None if n > 0 =>
      val ys = new TeeIterator(n)(xs)
      for (h <- rowParser.parseHeader(ys.tee); t <- parseRows(ys, h)) yield t
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
      bad foreach failureHandler //AbstractTableParser.logException[Row]
      FP.sequence(good filter predicate)
    }
    else
      FP.sequence(rys filter predicate)

    val q: Seq[Try[Row]] = mapTsToRows.toSeq
    for (rs <- processTriedRows(q.iterator)) yield builder(rs.toList, header)
  }
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

  def parseRows(xs: Iterator[String], header: Header): Try[Table] = doParseRows(xs, header, rowParser.parse)
}

/**
 * Abstract class to extend AbstractTableParser but with Input = Strings
 * This is the unusual situation where a file is a sequence of a sequence of Strings, each representing one value.
 *
 * @tparam Table the table type.
 */
abstract class StringsTableParser[Table] extends AbstractTableParser[Table] {
  type Input = Strings

  def parseRows(xs: Iterator[Strings], header: Header): Try[Table] = doParseRows(xs, header, rowParser.parse)
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
