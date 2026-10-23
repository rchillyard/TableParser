/*
 * Copyright (c) 2026. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.util

import scala.compiletime.{constValue, erasedValue}

/**
 * Helpers for the type classes that are derived from a case class's Mirror
 * (CellParser, CsvRenderer, CsvGenerator, CsvProduct and HierarchicalRenderer).
 */
object Derivation {

  /**
   * Method to get the names of the fields of a case class from its Mirror's MirroredElemLabels.
   * This replaces 1.x's Reflection.extractFieldNames: the names come from the compiler,
   * and are exactly the constructor parameters, in declaration order.
   *
   * @tparam Ls the MirroredElemLabels.
   * @return the field names, in declaration order.
   */
  inline def fieldNames[Ls <: Tuple]: List[String] = inline erasedValue[Ls] match {
    case _: EmptyTuple => Nil
    case _: (l *: ls) => constValue[l].asInstanceOf[String] :: fieldNames[ls]
  }
}
