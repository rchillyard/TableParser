/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.{Header, Table, TableWithHeader}
import com.phasmidsoftware.util.FP

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
  val maybeFixedHeader: Option[Header]

  /**
    * Default method to create a new table.
    * It does this by invoking either builderWithHeader or builderWithoutHeader, as appropriate.
    *
    * @param rows   the rows which will make up the table.
    * @param header the Header, derived either from the program or the data.
    * @return an instance of Table.
    */
  def builder(rows: Seq[Row], header: Header): Table

  /**
    * Method to determine how errors are handled.
    *
    * @return true if individual errors are logged but do not cause parsing to fail.
    */
  def forgiving: Boolean = false

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
    * @return a Try[Table]
    */
  def parse(xs: Seq[Input]): Try[Table]

  /**
    * Method to log any failures (only in forgiving mode).
    *
    * @param rys the sequence of Try[Row]
    * @return a sequence of Try[Row] which will all be of type Success.
    */
  def logFailures(rys: Seq[Try[Row]]): Seq[Try[Row]]
}

/**
  * Case class to define a StringTableParser that assumes a header to be found in the input file.
  * This class attempts to provide as much built-in functionality as possible.
  *
  * This class assumes that the names of the columns are in the first line.
  * This class implements builder with a TableWithHeader object.
  * This class uses StandardRowParser of its rowParser.
  *
  * @param maybeFixedHeader None => requires that the data source has a header row.
  *                         Some(h) => specifies that the header is to be taken from h.
  *                         NOTE: that the simplest is to specify the header directly from the type X:
  * @see StringTableParserWithHeader#create
  * @tparam X the underlying row type which must provide evidence of a CellParser and ClassTag.
  */
case class StringTableParserWithHeader[X: CellParser : ClassTag](maybeFixedHeader: Option[Header] = None) extends StringTableParser[Table[X]] {
  type Row = X

  def builder(rows: Seq[Row], header: Header): Table[Row] = maybeFixedHeader match {
    case Some(h) => TableWithHeader(rows, h)
    case None => TableWithHeader(rows, Header[Row]()) // CHECK
  }

  val rowParser: RowParser[X, String] = StandardRowParser[X]
}

object StringTableParserWithHeader {
  /**
    * This create method constructs a StringTableParserWithHeader with header based simply on the type X.
    * In this case, the source data must have the same number of columns as X has parameters, and they must be in the
    * same order. Additionally, there should not be a header row in the source data.
    *
    * @tparam X the underlying type. There must be evidence of CellParser[X] and ClassTag[X].
    * @return a StringTableParserWithHeader[X].
    */
  def create[X: CellParser : ClassTag]: StringTableParserWithHeader[X] = StringTableParserWithHeader[X](Some(Header.apply[X]()))
}

abstract class AbstractTableParser[Table] extends TableParser[Table] {

  /**
    * Abstract method to parse a sequence of Inputs, with a given header.
    *
    * @param xs     the sequence of Inputs, one for each row
    * @param header the header to be used.
    * @return a Try[Table]
    */
  def parseRows(xs: Seq[Input], header: Header): Try[Table]

  /**
    * Method to parse a table based on a sequence of Inputs.
    *
    * @param xs the sequence of Inputs, one for each row
    * @return a Try[Table]
    */
  def parse(xs: Seq[Input]): Try[Table] = {
    def separateHeaderAndRows(h: Input, t: Seq[Input]): Try[Table] = for (ws <- rowParser.parseHeader(h); rs <- parseRows(t, ws)) yield rs

    //    if (rowParser == null) // XXX how can this happen?
    //      Failure(ParserException("implicit RowParser[Row] is undefined"))
    //    else
    maybeFixedHeader match {
      case Some(h) => parseRows(xs, h)
      case None => // NOTE: it is possible that we still don't really have a header encoded in the data either
        xs match {
          case h :: t => separateHeaderAndRows(h, t)
          //          case LazyList.cons(h, t) => separateHeaderAndRows(h, t)
          case _ => Failure(ParserException("no rows to parse"))
        }
    }
  }

  /**
    * Method to log any failures (only in forgiving mode).
    *
    * @param rys the sequence of Try[Row]
    * @return a sequence of Try[Row] which will all be of type Success.
    */
  def logFailures(rys: Seq[Try[Row]]): Seq[Try[Row]] = {
    def logException(e: Throwable): Unit = {
      val string = s"${e.getLocalizedMessage}${
        if (e.getCause == null) "" else s" caused by ${e.getCause.getLocalizedMessage}"
      }"
      // TODO this should be using a logger
      System.err.println(string)
    }

    val (good, bad) = rys.partition(_.isSuccess)
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

  //noinspection DuplicatedCode
  def parseRows(xs: Strings, header: Header): Try[Table] = {
    val rys = for (w <- xs) yield rowParser.parse(w)(header)
    for (rs <- FP.sequence(if (forgiving) logFailures(rys) else rys)) yield builder(rs, header)
  }
}

/**
  * Abstract class to extend AbstractTableParser but with Input = Strings
  * This is the unusual situation where a file is a sequence of a sequence of Strings, each representing one value.
  *
  * @tparam Table the table type.
  */
abstract class StringsTableParser[Table] extends AbstractTableParser[Table] {
  type Input = Strings

  def parseRows(xs: Seq[Strings], header: Header): Try[Table] = {
    // TODO merge with repeated code at line 147
    val rys = for (w <- xs) yield rowParser.parse(w)(header)
    for (rs <- FP.sequence(if (forgiving) logFailures(rys) else rys)) yield builder(rs, header)
  }
}

/**
  * TableParserHelper: abstract class to help with defining an implicit TableParser of Table[X].
  * Note that this class extends CellParser[X].
  * It is expected that this should be sub-classed by the object which is the companion object of X.
  * That will make it easiest for the compiler to discover the implicit value of type TableParser of Table[X]
  *
  * NOTE: this class should be used for simple cases where the the data and type X match according to one of options
  * for hasHeaderRow.
  * More complex situations can easily be handled but not using this TableParserHelper class.
  *
  * @param hasHeaderRow true (default) if the data to be read has an explicit header row with column names that match the parameters
  *                     of type X;
  *                     false if there is no header row in the data AND if the data has (unnamed) columns of the same number
  *                     and in the same order as defined by type X.
  * @tparam X the type for which we require a TableParser[X].
  */
abstract class TableParserHelper[X: ClassTag](hasHeaderRow: Boolean = true) extends CellParsers {

  /**
    * Abstract method which will return a CellParser[X].
    * NOTE that a typical definition will be something like cellParser2(Player.apply) where, in this case, the number
    * is 2 corresponding to the number of parameters in Player.
    *
    * @return
    */
  def cellParser: CellParser[X]

  implicit val xp: CellParser[X] = cellParser

  implicit val ptp: TableParser[Table[X]] = if (hasHeaderRow) StringTableParserWithHeader[X]() else StringTableParserWithHeader.create[X]
}