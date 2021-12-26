/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.table

import com.phasmidsoftware.parse.ParserException
import com.phasmidsoftware.render.CsvRenderer

import scala.util.{Failure, Try}

/**
  * Case class to represent a (raw) row from a table.
  *
  * @param ws  the (raw) Strings that make up the row.
  * @param hdr is the Header containing the column names.
  */
case class Row(ws: Seq[String], hdr: Header, index: Int) extends (String => Try[String]) {

  /**
    * Method to yield the value for a given column name
    *
    * NOTE this doesn't seem to be used.
    * TODO: why is ParserException not found to link to?
    *
    * @param w the column name.
    * @return the value as a String.
    */
  def apply(w: String): Try[String] = hdr.getIndex(w).flatMap { i => apply(i) }.recoverWith[String] { case _: IndexOutOfBoundsException => Failure[String](ParserException(s"Row: unknown column: $w")) }

  /**
    * Method to yield the xth element of this Row.
    *
    * @param x an index from 0 thru length-1.
    * @return the value as a String.
    */
  def apply(x: Int): Try[String] = Try(ws(x)) recoverWith {
    case e: IndexOutOfBoundsException if x == -1 => Failure(e)
    case _: IndexOutOfBoundsException => Failure(ParserException(s"Row: index out of range: $x (there are ${ws.size} elements)"))
  }

  /**
    * Method to get the index of a column name
    *
    * @param column the column name
    * @return the index, which might be -1
    */
  def getIndex(column: String): Int = hdr.getIndex(column).getOrElse(-1)

  override def toString(): String = s"""Row: ${ws.mkString("[", ",", "]")} with header=$hdr"""
}

object Row {
  implicit object CsvRendererRow extends CsvRenderer[Row] {
    val csvAttributes: CsvAttributes = implicitly[CsvAttributes]

    def render(r: Row, attrs: Map[String, String]): String = r.ws mkString csvAttributes.delimiter
  }

  /**
    * Method to yield a CsvGenerator[Row].
    *
    * @param hdr the Header.
    * @param c CsvAttributes.
    * @return a new CsvGenerator[Row].
    */
  def csvGenerator(hdr: Header)(implicit c: CsvAttributes): CsvGenerator[Row] = new CsvGenerator[Row] {
    val csvAttributes: CsvAttributes = c

    def toColumnName(po: Option[String], name: String): String = hdr.xs.mkString(c.delimiter)
  }

}

/**
  * A wrapper class to index a T.
  *
  * @param i the index (ordinal value).
  * @param t the instance of T.
  * @tparam T the underlying type.
  */
case class Indexed[T](i: Int, t: T)

object Indexed {
  def index[T](rows: Iterable[T]): Seq[Indexed[T]] = rows.toSeq.zipWithIndex.map(Indexed(_))

  def apply[T](tuple: (T, Int)): Indexed[T] = Indexed(tuple._2, tuple._1)
}