/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.RawRow
import com.phasmidsoftware.table.{HeadedTable, Header, Table}
import com.phasmidsoftware.util.FP
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import scala.util.{Failure, Try}

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
    * Default method to create a new table.
    * It does this by invoking either builderWithHeader or builderWithoutHeader, as appropriate.
    *
    * @param rows   the rows which will make up the table.
    * @param header the Header, derived either from the program or the data.
    * @return an instance of Table.
    */
  protected def builder(rows: Iterator[Row], header: Header): Table

  /**
    * Method to determine how errors are handled.
    *
    * @return true if individual errors are logged but do not cause parsing to fail.
    */
  protected val forgiving: Boolean = false

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
  def parse(xs: Iterator[Input]): Try[Table]

  /**
    * Method to log any failures (only in forgiving mode).
    *
    * @param rys the sequence of Try[Row]
    * @return a sequence of Try[Row] which will all be of type Success.
    */
  protected def logFailures(rys: Iterator[Try[Row]]): Iterator[Try[Row]]
}

object TableParser {
  val logger: Logger = LoggerFactory.getLogger(TableParser.getClass)


}

/**
  * Class used to parse files as a Table of Seq[String].
  * That's to say, no parsing of individual (or groups of) columns.
  *
  * @param maybeFixedHeader an optional fixed header. If None, we expect to find the header defined in the first line of the file.
  * @param forgiving        forcing (defaults to true). If true then an individual malformed row will not prevent subsequent rows being parsed.
  */
case class RawTableParser(maybeFixedHeader: Option[Header] = None, override val forgiving: Boolean = true) extends StringTableParser[Table[Seq[String]]] {
  type Row = RawRow

  implicit val stringSeqParser: CellParser[RawRow] = new CellParsers {}.cellParserSeq

  val rowParser: RowParser[Row, String] = StandardRowParser[RawRow]

  // CONSIDER why do we have a concrete Table type mentioned here?
  protected def builder(rows: Iterator[Row], header: Header): Table[Row] = HeadedTable(rows, header)
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
  *                         NOTE: that the simplest is to specify the header directly from the type X:
  * @see HeadedStringTableParser#create
  * @tparam X the underlying row type which must provide evidence of a CellParser and ClassTag.
  */
case class HeadedStringTableParser[X: CellParser : ClassTag](maybeFixedHeader: Option[Header] = None, override val forgiving: Boolean = false) extends StringTableParser[Table[X]] {
  type Row = X


  protected def builder(rows: Iterator[X], header: Header): Table[Row] = maybeFixedHeader match {
    case Some(h) => HeadedTable(rows, h)
    case None => HeadedTable(rows, Header[Row]()) // CHECK
  }

  protected val rowParser: RowParser[X, String] = StandardRowParser[X]
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
  def create[X: CellParser : ClassTag](forgiving: Boolean): HeadedStringTableParser[X] = HeadedStringTableParser[X](Some(Header.apply[X]()), forgiving)
}

/**
  * Abstract base class for implementations of TableParser[T].
  * NOTE: that Table is a parametric type and does NOT refer to the type Table defined elsewhere.
  *
  * @tparam Table the (parametric) Table type.
  */
abstract class AbstractTableParser[Table] extends TableParser[Table] {

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
    * @param xs the sequence of Inputs, one for each row
    * @return a Try[Table]
    */
  def parse(xs: Iterator[Input]): Try[Table] = {
    def separateHeaderAndRows(h: Input, t: Iterable[Input]): Try[Table] =
      for (ws <- rowParser.parseHeader(h); rs <- parseRows(t.iterator, ws)) yield rs

    maybeFixedHeader match {
      case Some(h) => parseRows(xs, h)
      case None => // NOTE: it is possible that we still don't really have a header encoded in the data either
        val seq = xs.toList
        seq match {
          case h :: t =>
            separateHeaderAndRows(h, t)
          case _ => Failure(ParserException("no rows to parse"))
        }
    }
  }

  /**
    * Common code for parsing rows.
    *
    * @param ts     a sequence of Ts.
    * @param header the Header.
    * @param f      a curried function which transforms a T into a function which is of type Header => Try[Row].
    * @tparam T the parametric type of the resulting Table. T corresponds to Input in the calling method, i.e. a Row.
    * @return a Try of Table
    */
  protected def doParseRows[T](ts: Iterator[T], header: Header, f: T => Header => Try[Row]): Try[Table] = {
    val rys = for (t <- ts) yield f(t)(header)
    for (rs <- FP.sequence(if (forgiving) logFailures(rys) else rys)) yield builder(rs, header)
  }

  /**
    * Method to log any failures (only in forgiving mode).
    *
    * @param rys the sequence of Try[Row]
    * @return a sequence of Try[Row] which will all be of type Success.
    */
  protected def logFailures(rys: Iterator[Try[Row]]): Iterator[Try[Row]] = {
    def logException(e: Throwable): Unit = {
      val string = s"${e.getLocalizedMessage}${
        if (e.getCause == null) "" else s" caused by ${e.getCause.getLocalizedMessage}"
      }"
      TableParser.logger.warn(string)
    }

    val (good, bad) = rys.partition(_.isSuccess)
    if (bad.isEmpty) TableParser.logger.warn("forgiving mode is set but there are no failures")
    bad.map(_.failed.get) foreach (e => logException(e))
    good
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

  implicit val ptp: TableParser[Table[X]] = if (sourceHasHeaderRow) HeadedStringTableParser[X](None, forgiving) else HeadedStringTableParser.create[X](forgiving)
}

case class TableParserException(msg: String, e: Option[Throwable] = None) extends Exception(msg, e.orNull)