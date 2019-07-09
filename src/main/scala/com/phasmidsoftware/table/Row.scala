/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.table

import com.phasmidsoftware.parse.ParserException

/**
  * Case class to represent a (raw) row from a table.
  *
  * @param ws  the (raw) Strings that make up the row.
  * @param hdr is the column names, which should all be in upper case.
  */
case class Row(ws: Seq[String], hdr: Header) extends (String => String) {

  /**
    * Method to yield the value for a given column name
    *
    * NOTE this doesn't seem to be used.
    *
    * @param w the column name.
    * @return the value as a String.
    * @throws ParserException if w is not contained in hdr.
    */
  def apply(w: String): String = try apply(hdr.getIndex(w)) catch {
    case _: IndexOutOfBoundsException => throw ParserException(s"Row: unknown column: $w")
  }

  /**
    * Method to yield the xth element of this Row.
    *
    * @param x an index from 0 thru length-1.
    * @return the value as a String.
    * @throws ParserException if x is out of range.
    */
  def apply(x: Int): String = try ws(x) catch {
    case e: IndexOutOfBoundsException if x == -1 => throw e
    case _: IndexOutOfBoundsException => throw ParserException(s"Row: index out of range: $x (there are ${ws.size} elements)")
  }

  /**
    * Method to get the index of a column name
    *
    * @param column the column name
    * @return the index, which might be -1
    */
  def getIndex(column: String): Int = hdr.getIndex(column)

  override def toString(): String = s"""Row: ${ws.mkString("[", ",", "]")} with header=$hdr"""
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
  def index[T](rows: Seq[T]): Seq[Indexed[T]] = rows.zipWithIndex.map(Indexed(_))

  def apply[T](tuple: (T, Int)): Indexed[T] = Indexed(tuple._2, tuple._1)
}