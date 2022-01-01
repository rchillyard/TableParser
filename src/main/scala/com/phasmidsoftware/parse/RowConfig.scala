/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import scala.util.matching.Regex

/**
  * Trait to define the configuration for parsing a row.
  */
trait RowConfig {
  /**
    * the delimiter Regex (see LineParser). defaults to ", *".r, i.e. a comma followed by any n=umber of spaces.*
    */
  val delimiter: Regex
  /**
    * the "string" Regex (see LineParser). defaults to "\w+".r, i.e. at least one word character.
    */
  val string: Regex
  /**
    * the "listSep" character (see LineParser). defaults to "|"
    */
  val listSep: Char
  /**
    * the "listEnclosure" characters (see LineParser). defaults to "{}"
    */
  val listEnclosure: String
  /**
    * the "quote" Char (see LineParser). defaults to ".
    */
  val quote: Char

  override def toString: String = s"RowConfig: delimiter='$delimiter', string='$string', listSep='$listSep', listEnclosure='$listEnclosure', $quote='$quote'"
}

/**
  * Default RowConfig trait.
  */
trait DefaultRowConfig extends RowConfig {
  /**
    * the "listSep" character (see LineParser). defaults to "|"
    */
  val listSep: Char = '|'
  /**
    * the "listEnclosure" characters (see LineParser). defaults to "{}"
    */
  val listEnclosure: String = "{}"
  /**
    * the delimiter Regex (see LineParser). defaults to ", *".r, i.e. a comma followed by any n=umber of spaces.*
    */
  val delimiter: Regex = """\s*,\s*""".r
  /**
    * the "string" Regex (see LineParser). defaults to "\w+".r, i.e. at least one word character.
    * CONSIDER making the string regex derive from the delimiter
    */
  val string: Regex =
    """[^,"]*""".r
  /**
    * the "quote" Char (see LineParser). defaults to ".
    */
  val quote: Char = '"'
}

/**
  * Companion object to RowConfig.
  */
object RowConfig {

  // CONSIDER removing this default row configuration. It might be better to have the compiler issue a warning when it's missing.
  implicit object defaultRowConfig extends DefaultRowConfig

}
