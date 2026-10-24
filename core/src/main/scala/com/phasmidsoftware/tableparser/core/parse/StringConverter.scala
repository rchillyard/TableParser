package com.phasmidsoftware.tableparser.core.parse

import scala.util.Try

/**
 * Trait to convert a String into a T (see CellParser, which extends it).
 *
 * NOTE: up to 2.0, the companion object had instances for the basic types, but nothing used them
 * (CellParser's own instances are found instead).
 *
 * @tparam T the type of the result.
 */
trait StringConverter[+T]:

  /**
   * Convert a String into a T.
   *
   * @param w the String to be converted.
   * @return a new instance of T wrapped in Try
   */
  def convertString(w: String): Try[T]

