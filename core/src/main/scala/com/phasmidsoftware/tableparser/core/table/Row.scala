/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.table

import com.phasmidsoftware.tableparser.core.parse.{ParserException, RowValues}
import com.phasmidsoftware.tableparser.core.render.{CsvGenerator, CsvRenderer}
import scala.util.{Failure, Try}

/**
 * Case class to represent a (raw) row from a table.
 *
 * @param ws  the (raw) Strings that make up the row.
 * @param hdr is the Header containing the column names.
 */
case class Row(ws: Seq[String], hdr: Header, index: Int) extends BaseRow(ws, hdr) {
  override def toString(): String =
    s"""Row: $index: ${ws.mkString("[", ",", "]")} with header=$hdr"""
}

/**
 * Abstract class to represent a (raw) row from a table.
 *
 * @param ws  the (raw) Strings that make up the row.
 * @param hdr is the Header containing the column names.
 */
abstract class BaseRow(ws: Seq[String], hdr: Header) extends (String => Try[String]) {

  /**
   * Method to yield the value for a given column name
   *
   * CONSIDER: why is ParserException not found to link to?
   *
   * @param w the column name.
   * @return the value as a String.
   */
  def apply(w: String): Try[String] =
    hdr.getIndex(w).flatMap { i => apply(i) }.recoverWith[String] { case _: IndexOutOfBoundsException => Failure[String](ParserException(s"Row: unknown column: $w")) }

  /**
   * Method to yield the xth element of this Row.
   *
   * @param x an index from 0 thru length-1.
   * @return the value as a String.
   */
  def apply(x: Int): Try[String] = Try(ws(x)) recoverWith {
    case e: IndexOutOfBoundsException if x == -1 =>
      Failure(e)
    case _: IndexOutOfBoundsException =>
      Failure(ParserException(s"Row: index out of range: $x (there are ${ws.size} elements)"))
  }

  /**
   * Method to get the index of a column name
   *
   * @param column the column name
   * @return the index, which might be -1
   */
  def getIndex(column: String): Int =
    hdr.getIndex(column).getOrElse(-1)

  /**
   * Generates a string representation of the row, including its values and header information.
   *
   * @return a string in the format "Row: [value1, value2, ...] with header=HeaderObject".
   */
  override def toString(): String =
    s"""Row: ${ws.mkString("[", ",", "]")} with header=$hdr"""
}

/**
 * Companion object for the `Row` class, providing utilities for working with rows,
 * including CSV rendering and generation.
 */
object Row {
  /**
   * Implicit object that provides a CSV rendering implementation for the `Row` class.
   *
   * This object extends the `CsvRenderer` type class for `Row`, enabling rows to be
   * rendered as CSV strings. The default behavior concatenates the row elements (`ws`)
   * into a single string, separated by the delimiter defined in the implicit `CsvAttributes`.
   *
   * @see CsvRenderer
   * @see Row
   */
  implicit object CsvRendererRow extends CsvRenderer[Row] {
    val csvAttributes: CsvAttributes = implicitly[CsvAttributes]

    def render(r: Row, attrs: Map[String, String]): String = r.ws mkString csvAttributes.delimiter
  }

  /**
   * Method to yield a CsvGenerator[T] from an instance of Header.
   *
   * @param hdr the Header.
   * @param c   CsvAttributes.
   * @return a new CsvGenerator[T].
   */
  def csvGenerator[T](hdr: Header)(implicit c: CsvAttributes): CsvGenerator[T] = new CsvGenerator[T] {
    val csvAttributes: CsvAttributes = c

    /**
     * Converts the given optional prefix and column name into a final column name string.
     * The method combines different parts of a header using a specified delimiter and returns
     * the resulting string representation.
     *
     * @param po   An optional string that may act as a prefix for the column name. If present,
     *             it is prepended to the column name.
     * @param name The actual column name to be processed.
     * @return A string representing the fully qualified column name based on the header and delimiter configuration.
     */
    def toColumnName(po: Option[String], name: String): String = ((hdr.xs +: hdr.xss) map (_.mkString(c.delimiter))).mkString("\n")
  }
}

case class RawRow(ws: Seq[String], header: Header) extends BaseRow(ws, header) {
  override def toString(): String = RowValues(Row(ws, header, 0), header).toString
//    s"""RawRow: ${ws.mkString("[", ",", "]")} with header=$header"""
}

/**
 * A wrapper class to index a T.
 *
 * @param i the index (ordinal value).
 * @param t the instance of T.
 * @tparam T the underlying type.
 */
case class Indexed[T](i: Int, t: T)

/**
 * Companion object for the `Indexed` case class, providing utility methods
 * to create and work with indexed collections.
 */
object Indexed {
  /**
   * Creates a sequence of `Indexed` instances by associating each element of the input
   * collection with a zero-based index.
   *
   * @param rows the input collection of elements to be indexed
   * @tparam T the type of elements in the input collection
   * @return a sequence of `Indexed` instances, where each element is paired with its index
   */
  def index[T](rows: Iterable[T]): Seq[Indexed[T]] =
    rows.toSeq.zipWithIndex.map(Indexed(_))

  /**
   * Creates an `Indexed` instance from a tuple containing a value of type `T` and an integer index.
   *
   * @param tuple A tuple where the first element is of type `T` and the second element is an `Int` representing the index.
   * @tparam T The type of the first element in the tuple.
   * @return An `Indexed` instance containing the provided index and value, with the order of elements reversed.
   */
  def apply[T](tuple: (T, Int)): Indexed[T] =
    Indexed(tuple._2, tuple._1)
}