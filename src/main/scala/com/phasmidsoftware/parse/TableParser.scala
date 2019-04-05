/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.Header

import scala.util.{Failure, Try}

trait TableParser[Table] {

  type Row

  type Input

  def hasHeader: Boolean

  def forgiving: Boolean = false

  def rowParser: RowParser[Row, Input]

  def builder(rows: Seq[Row]): Table
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
    for (rs <- FP.sequence(if (forgiving) logFailures(rys) else rys)) yield builder(rs)
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
    for (rs <- FP.sequence(if (forgiving) logFailures(rys) else rys)) yield builder(rs)
  }
}