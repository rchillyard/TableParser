/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import scala.annotation.implicitNotFound

/**
 * Type class representing a mapping from a case class parameter to the corresponding column header.
 *
 * @tparam T the type of the object being helped.
 */
@implicitNotFound(msg = "Cannot find an implicit instance of ColumnHelper[${T}]. If ${T} is a case class, you will normally need to provide some help translating column names.")
trait ColumnHelper[T] {
  /**
   * This is the format for the prefix of a name, where "x" represents the value, if any, of the optional string.
   */
  val maybePrefix_ : Option[String]

  /**
   * These are the alias mappings
   */
  val aliases_ : Seq[(String, String)]

  /**
   * This defines the default mapping between parameter names and column names.
   * It is only used if there is no mapping defined in aliases.
   *
   * @return the mapper function.
   */
  val columnNameMapper_ : String => String

  /**
   * This is the lookup function
   *
   * @param so is an optional string to be inserted into the prefix
   * @param w  is the name of column, as determined from the Product which we are trying to fill.
   * @return a String which may include a prefix.
   */
  def lookup(so: Option[String], w: String): String = {
    val column = aliases_.toMap.getOrElse(w, columnNameMapper_(w))
    (for (p <- maybePrefix_; s <- so) yield p.replace("$x", s).replace("$c", column)).getOrElse(column)
  }
}

object ColumnHelper {
  def camelCaseColumnNameMapper(w: String): String = w.replaceAll("([A-Z0-9])", "_$1")

  def camelCaseColumnNameMapperSpace(w: String): String = w.replaceAll("([A-Z0-9])", " $1")
}