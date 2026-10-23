/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core

package object parse {

  /**
   * type alias for the results of parsing repetitions of String.
   */
  type StringList = List[String]

  /**
   * type alias for parsing rows which are composed of a sequence of String.
   */
  type Strings = Seq[String]

  // CONSIDER moving this definition and renaming it
  def cellReader[T](using cellParser: CellParser[T]): CellParser[T] = cellParser

}
