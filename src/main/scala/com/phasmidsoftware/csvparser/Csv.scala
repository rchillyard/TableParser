package com.phasmidsoftware.csvparser

import scala.util.{Failure, Try}


trait CsvParser {

  def hasHeader: Boolean

  def rowParser: RowParser

  def parse(ws: Seq[String]): Try[Csv] = {
    def parseRows(header: Seq[String], ws1: Seq[String]): Try[Csv] = for (rs <- FP.sequence(for (w <- ws1) yield rowParser.parse(w)(header))) yield Csv(rs)

    if (hasHeader) ws match {
      case h :: t => parseRows(rowParser.parseHeader(h), t)
      case _ => Failure(RowException("no rows to parse"))
    }
    else parseRows(Nil, ws)
  }

}

case class Csv(rows: Seq[Row]) {


}

object FP {
  /**
    *
    * @param xys a sequence of Try[X]
    * @tparam X the underlying type
    * @return a Try of Seq[X]
    *         NOTE: that the output collection type will be Seq, regardless of the input type
    */
  def sequence[X](xys: Seq[Try[X]]): Try[Seq[X]] = (Try(Seq[X]()) /: xys) {
    (xsy, xy) => for (xs <- xsy; x <- xy) yield xs :+ x
  }

}