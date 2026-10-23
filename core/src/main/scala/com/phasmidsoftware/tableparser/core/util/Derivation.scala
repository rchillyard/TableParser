/*
 * Copyright (c) 2026. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.util

import scala.compiletime.{constValue, erasedValue}
import scala.deriving.Mirror

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

/**
 * Type class giving the names of the fields of a case class T, in declaration order.
 * An instance is derived (from T's Mirror) wherever T is a known case class.
 * Generic code which needs the field names of its type parameter (e.g. to make a Header) asks for a FieldNames[T];
 * this replaces 1.x's use of a ClassTag[T] and reflection.
 *
 * @tparam T the case class.
 */
trait FieldNames[T] {
  def names: List[String]
}

object FieldNames {
  /**
   * The FieldNames of a case class T, from its Mirror.
   *
   * @param m the Mirror of T.
   * @tparam T a case class.
   * @return a FieldNames[T].
   */
  inline given derived[T](using m: Mirror.ProductOf[T]): FieldNames[T] =
    FieldNamesOf[T](Derivation.fieldNames[m.MirroredElemLabels])
}

/**
 * A FieldNames[T] with the given names.
 *
 * @param names the field names.
 * @tparam T the case class.
 */
case class FieldNamesOf[T](names: List[String]) extends FieldNames[T]
