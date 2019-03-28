package com.phasmidsoftware.parse

import com.phasmidsoftware.table.Header

import scala.util.{Failure, Try}

trait TableParser[Table] {

  type Row

  def hasHeader: Boolean

  def forgiving: Boolean = false

  def rowParser: RowParser[Row]

  def builder(rows: Seq[Row]): Table


  // CONSIDER returning Table unwrapped and using lifted conversion functions
  def parse(ws: Seq[String]): Try[Table] = {
    // CONSIDER swap order of params
    def parseRows(header: Header, ws1: Seq[String]): Try[Table] = {
      val rys = for (w <- ws1) yield rowParser.parse(w)(header)
      for (rs <- FP.sequence(if (forgiving) logFailures(rys) else rys)) yield builder(rs)
    }

    def logException(e: Throwable): Unit = {
      val string = s"${e.getLocalizedMessage}${
        if (e.getCause == null) "" else s" caused by ${e.getCause.getLocalizedMessage}"
      }"
      // TODO this should be using a logger
      System.err.println(string)
    }

    def logFailures(rys: Seq[Try[Row]]): Seq[Try[Row]] = {
      val (good, bad) = rys.partition(_.isSuccess)
      bad.map(_.failed.get) foreach (e => logException(e))
      good
    }

    def separateHeaderAndRows(h: String, t: Seq[String]) = for (ws <- rowParser.parseHeader(h); rs <- parseRows(ws, t)) yield rs

    if (rowParser == null) Failure(ParserException("implicit RowParser[Row] is undefined"))

    else if (hasHeader) ws match {
      case h #:: t => separateHeaderAndRows(h, t)
      case h :: t => separateHeaderAndRows(h, t)
      case _ => Failure(ParserException("no rows to parse"))
    }
    else parseRows(Header(Nil), ws)
  }
}
