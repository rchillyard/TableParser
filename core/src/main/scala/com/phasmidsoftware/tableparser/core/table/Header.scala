/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.table

import com.phasmidsoftware.tableparser.core.util.FP._
import com.phasmidsoftware.tableparser.core.util.FieldNames
import scala.language.postfixOps
import scala.util.Try

/**
 * Case class to represent a header.
 *
 * A Header is normally read from the first row(s) of a file (see TableParser.headerRowsToRead).
 * Otherwise, a fixed Header can be given to a TableParser (e.g. via setHeader), made in one of these ways:
 *  - Header.create("a", "b", "c"): the given column names;
 *  - Header[X](): the names of the fields of the case class X;
 *  - Header(letters = true, n): n columns named as in a spreadsheet (A B ... Z AA AB ...), or by number (1 2 3 ...);
 *  - Header(wss): from several rows, the first of which gives the column names.
 *
 * @param xs  the sequence of column names.
 * @param xss a sequence of sequence of String representing any additional header lines.
 */
case class Header(xs: Seq[String], xss: Seq[Seq[String]]) {
  /**
   * Get the number of columns.
   *
   * @return the size of the header sequence.
   */
  def size: Int =
    xs.length

  /**
   * Get the index of w in this Header.
   * A column whose name is exactly w is preferred; only if there is none is case ignored.
   * So, if two columns differ only in case, the exact one is found, and a table which TableParser writes
   * (with exact column names) is read back exactly.
   *
   * @param w the String to find, a column name.
   * @return the index wrapped in Try.
   */
  def getIndex(w: String): Try[Int] =
    indexFound(w, xs.indexOf(w) match {
      case -1 => xs.indexWhere(x => x.compareToIgnoreCase(w) == 0)
      case i => i
    })

  /**
   * Concatenate this Header with other.
   *
   * The additional header lines are joined pairwise (a missing line counts as empty).
   *
   * @param other the other Header.
   * @return a Header made up of these columns and those of other, in that order.
   */
  def ++(other: Header): Header =
    Header(xs ++ other.xs, xss.zipAll(other.xss, Nil, Nil) map ((ws, vs) => ws ++ vs))
}

/**
 * This companion object contains utility methods and constants
 * for creating and manipulating instances of the Header class.
 */
object Header {

  private lazy val numbers: LazyList[Int] = LazyList.from(1)

  /**
   * Column names as in a spreadsheet, by number: 1 2 3 ...
   */
  lazy val generateNumbers: LazyList[String] = numbers map (_.toString)

  /**
   * Column names as in a spreadsheet, by letter: A B C ... Z AA AB ... AZ BA ... ZZ AAA ...
   */
  lazy val generateLetters: LazyList[String] = numbers map intToString(letters = true)

  // The column name of the nth column (from 1): with letters, n is written in "bijective base 26" (A=1 ... Z=26).
  private def intToString(letters: Boolean)(n: Int): String = if (letters) {
    @scala.annotation.tailrec
    def inner(s: String, n: Int): String =
      if (n <= 0) s
      else {
        val m = if (n % 26 == 0) 26 else n % 26
        inner(s"${(m + 64).toChar}$s", (n - m) / 26)
      }

    inner("", n)
  }
  else n.toString

  /**
   * This method constructs a new Header based on a header spanning several lines.
   * Only the first becomes the true header, the remainder become the "preface."
   *
   * @param wss a sequence of sequence of String.
   * @return a Header.
   */
  def apply(wss: Seq[Seq[String]]): Header =
    apply(wss.head, wss.tail)

  /**
   * This method constructs a new Header based on Excel row/column names.
   *
   * @param letters true if we want the sequence A B C D E ... Z AA AB ... BA BB ...
   *                false is we just want numbers.
   * @param length  the number of columns.
   * @return a Header with length column names.
   */
  def apply(letters: Boolean, length: Int): Header =
    Header(numbers map intToString(letters) take length toList, Nil)

  /**
   * This method constructs a new Header from the names of the fields of the case class X.
   * It should not be considered to be the normal method of generating the header for a table.
   * It is mainly used by unit tests.
   *
   * NOTE: this method does NOT recurse into any fields which happen also to be a class with fields.
   *
   * @param fieldNames the FieldNames of X (derived from its Mirror).
   * @tparam X a case class.
   * @return a Header with the field names of X.
   */
  def apply[X]()(using fieldNames: FieldNames[X]): Header =
    Header(fieldNames.names, Nil)

  /**
   * Create a Header from a variable list of parameters.
   *
   * @param ws a variable list of Strings.
   * @return a Header.
   */
  def create(ws: String*): Header =
    apply(ws, Nil)
}
