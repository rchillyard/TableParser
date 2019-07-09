/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.Header
import com.phasmidsoftware.util.FP

import scala.annotation.implicitNotFound
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
    * Default method to create a new table.
    * It does this by invoking either builderWithHeader or builderWithoutHeader, as appropriate.
    *
    * @param rows        the rows which will make up the table.
    * @param maybeHeader an optional Header.
    * @return an instance of Table.
    */
  def builder(rows: Seq[Row], maybeHeader: Option[Header]): Table = if (hasHeader)
    builderWithHeader(rows, maybeHeader.getOrElse(Header(Nil)))
  else
    builderWithoutHeader(rows)

  /**
    * NOTE: if hasHeader yields true, this method MUST be overridden and implemented.
    *
    * @param rows   the rows to beuild into the table.
    * @param header the header to use for the table.
    * @return a new instance of Table.
    */
  def builderWithHeader(rows: Seq[Row], header: Header): Table = throw ParserException(s"No builderWithHeader method implemented for this TableParser with hasHeader=true")

  /**
    * NOTE: if hasHeader yields false, this method MUST be overridden and implemented.
    *
    * NOTE: even if hasHeader yields true, it may be that a table has no header.
    *
    * @param rows the rows to beuild into the table.
    * @return a new instance of Table.
    */
  def builderWithoutHeader(rows: Seq[Row]): Table = throw ParserException(s"No builderWithoutHeader method implemented for this TableParser with hasHeader=false")

  /**
    * NOTE: this method must be consistent with the builder methods below.
    *
    * @return true if this table parser should provide a header.
    */
  def hasHeader: Boolean

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
  def rowParser: RowParser[Row, Input]
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

    if (rowParser == null)
      Failure(ParserException("implicit RowParser[Row] is undefined"))
    else if (hasHeader) xs match {
      case h #:: t => separateHeaderAndRows(h, t)
      case h :: t => separateHeaderAndRows(h, t)
      case _ => Failure(ParserException("no rows to parse"))
    }
    else parseRows(xs, Header(Nil))
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
  * Abstract class to extend AbstractTableParser but with Input = String
  *
  * @tparam Table the table type.
  */
abstract class StringTableParser[Table] extends AbstractTableParser[Table] {
  type Input = String

  def parseRows(xs: Strings, header: Header): Try[Table] = {
    val rys = for (w <- xs) yield rowParser.parse(w)(header)
    //		System.err.println(s"StringTableParser: parsed ${rys.size} of ${xs.size} rows")
    for (rs <- FP.sequence(if (forgiving) logFailures(rys) else rys)) yield builder(rs, Some(header))
  }

}

/**
  * Abstract class to extend AbstractTableParser but with Input = Strings
  *
  * @tparam Table the table type.
  */
abstract class StringsTableParser[Table] extends AbstractTableParser[Table] {
  type Input = Strings

  def parseRows(xs: Seq[Strings], header: Header): Try[Table] = {
    val rys = for (w <- xs) yield rowParser.parse(w)(header)
    //		System.err.println(s"StringsTableParser: parsed ${rys.size} of ${xs.size} rows")
    for (rs <- FP.sequence(if (forgiving) logFailures(rys) else rys)) yield builder(rs, Some(header))
  }

}