package com.phasmidsoftware.parse

trait ColumnHelper[T] {
  /**
    * This is the format for the prefix of a name, where "x" represents the value, if any, of the optional string.
    */
  val _maybePrefix: Option[String]

  /**
    * These are the alias mappings
    */
  val _aliases: Seq[(String, String)]

  /**
    * This defines the default mapping between parameter names and column names.
    * It is only used if there is no mapping defined in aliases.
    *
    * @return the mapper function.
    */
  val _columnNameMapper: String => String

  /**
    * This is the lookup function
    *
    * @param so is an optional string to be inserted into the prefix
    * @param w  is the name of column, as determined from the Product which we are trying to fill.
    * @return a String which may include a prefix.
    */
  def lookup(so: Option[String], w: String): String = {
    val column = _aliases.toMap.getOrElse(w, _columnNameMapper(w))
    (for (prfx <- _maybePrefix; s <- so) yield prfx.replace("$x", s).replace("$c", column)).getOrElse(column)
  }
}
