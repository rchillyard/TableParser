/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import scala.util.matching.Regex

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

trait DefaultRowConfig extends RowConfig {
  /**
    * the delimiter Regex (see LineParser). defaults to ", *".r, i.e. a comma followed by any n=umber of spaces.*
    */
  val delimiter: Regex = ", *".r
  /**
    * the "string" Regex (see LineParser). defaults to "\w+".r, i.e. at least one word character.
    * CONSIDER making the string regex derive from the delimiter
    */
  val string: Regex = """[^\,]*""".r

  /**
    * the "listSep" character (see LineParser). defaults to "|"
    */
  override val listSep: Char = '|'
  /**
    * the "listEnclosure" characters (see LineParser). defaults to "{}"
    */
  override val listEnclosure: String = "{}"
  /**
    * the "quote" Char (see LineParser). defaults to ".
    */
  val quote: Char = '"'
}


object RowConfig {

  implicit object defaultRowConfig extends DefaultRowConfig

}
