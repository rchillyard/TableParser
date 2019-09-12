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
    * This variable determines if there is a programmed header for the parser.
    * If its value is None, it signifies that we must look to the first line of data
    * for an appropriate header.
    */
  val maybeHeader: Option[Header]

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

    if (rowParser == null) // XXX how can this happen?
      Failure(ParserException("implicit RowParser[Row] is undefined"))
    else maybeHeader match {
      case Some(h) => parseRows(xs, h)
      case None => // NOTE: it is possible that we still don't really have a header encoded in the data either
      xs match {
      case h #:: t => separateHeaderAndRows(h, t)
      case h :: t => separateHeaderAndRows(h, t)
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
  * This is the unuual situation where a file is a sequence of a sequence of Strings, each representing one value.
  *
  * @tparam Table the table type.
  */
abstract class StringsTableParser[Table] extends AbstractTableParser[Table] {
  type Input = Strings

  def parseRows(xs: Seq[Strings], header: Header): Try[Table] = {
    // TODO merge with repeated code at line 156
    val rys = for (w <- xs) yield rowParser.parse(w)(header)
    for (rs <- FP.sequence(if (forgiving) logFailures(rys) else rys)) yield builder(rs, header)
  }

}