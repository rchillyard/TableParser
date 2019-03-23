package com.phasmidsoftware.table

import com.phasmidsoftware.parse.ParserException

/**
  * Case class to represent a (raw) row from a table.
  *
  * @param ws  the (raw) Strings that make up the row.
  * @param hdr is the column names, which should all be in upper case.
  */
case class Row(ws: Seq[String], hdr: Seq[String]) extends (String => String) {

  /**
    * Method to yield the xth element of this Row.
    *
    * @param x an index from 0 thru length-1.
    * @return the value as a String.
    * @throws ParserException if x is out of range.
    */
  def apply(x: Int): String = try ws(x) catch {
    case _: IndexOutOfBoundsException => throw ParserException(s"Row: index out of range: $x")
  }

  /**
    * Method to yield the value for a given column name
    *
    * @param w the column name.
    * @return the value as a String.
    * @throws ParserException if w is not contained in hdr.
    */
  def apply(w: String): String = try ws(getIndex(w)) catch {
    case _: IndexOutOfBoundsException => throw ParserException(s"Row: unknown column: $w")
  }

  private def getIndex(w: String) = hdr.indexOf(w.toUpperCase)
}
