package com.phasmidsoftware.tableparser

import java.io.File
import java.net.URI

import com.phasmidsoftware.csvparser._

import scala.io.Source
import scala.util.{Failure, Try}

trait TableParser[Table] {

  type Row

  def hasHeader: Boolean

  def rowParser: RowParser[Row]

  def builder(rows: Seq[Row]): Table

    def parse(ws: Seq[String]): Try[Table] = {
      def parseRows(header: Seq[String], ws1: Seq[String]): Try[Table] = for (rs <- FP.sequence(for (w <- ws1) yield rowParser.parse(w)(header))) yield builder(rs)

      if (hasHeader) ws match {
        case h :: t => parseRows(rowParser.parseHeader(h), t)
        case _ => Failure(RowException("no rows to parse"))
      }
      else parseRows(Nil, ws)
    }
}

trait Table[Row] extends Iterable[Row] {

  def maybeHeader: Option[Seq[String]]

  def map[S](f: Row => S): Table[S] = unit(rows map f, maybeHeader)

  def flatMap[U](f: Row => Table[U]): Table[U] = (rows map f).foldLeft(unit[U](Nil,None))(_ ++ _)

  def unit[S](rows: Seq[S], maybeHeader: Option[Seq[String]]): Table[S]

  def ++[U >: Row](table: Table[U]): Table[U] = unit[U](rows ++ table.rows, for (h1 <- maybeHeader; h2 <- table.maybeHeader) yield h1++h2)

  def rows: Seq[Row]

  def iterator: Iterator[Row] = rows.iterator
}

object Table {

  def parse[T : TableParser](ws: Seq[String]): Try[T] = implicitly[TableParser[T]].parse(ws)

  def parse[T : TableParser](ws: Iterator[String]): Try[T] = parse(ws.toSeq)

  def parse[T : TableParser](x: Source): Try[T] = parse(x.getLines())

  def parse[T : TableParser](u: URI): Try[T] = for (s <- Try(Source.fromURI(u)); t <- parse(s)) yield t

  def parse[T : TableParser](f: File): Try[T] = for (s <- Try(Source.fromFile(f)); t <- parse(s)) yield t
}

/**
  * CONSIDER eliminating this base class
  * @param rows
  * @param maybeHeader
  * @tparam Row
  */
abstract class BaseTable[Row](rows: Seq[Row], val maybeHeader: Option[Seq[String]]) extends Table[Row]{
  self =>

}

case class TableWithHeader[Row](rows: Seq[Row], header: Seq[String]) extends BaseTable[Row](rows, Some(header)){
  override def unit[S](rows: Seq[S], maybeHeader: Option[Seq[String]]): Table[S] = maybeHeader match { case Some(h) => TableWithHeader(rows,h); case _ => throw TableException("header is non-existent") }
}

case class TableWithoutHeader[Row](rows: Seq[Row]) extends BaseTable[Row](rows, None){
  override def unit[S](rows: Seq[S], maybeHeader: Option[Seq[String]]): Table[S] = maybeHeader match {case None => TableWithoutHeader(rows); case _ => throw TableException("header should be non-existent") }
}

case class TableException(w: String) extends Exception(w)