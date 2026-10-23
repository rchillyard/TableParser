/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.parse

import scala.util.matching.Regex

/**
 * Trait to define the configuration for parsing a row.
 */
trait RowConfig {
  /**
   * the delimiter Regex (see LineParser). defaults to ",".r, i.e., a comma, as in RFC 4180:
   * spaces next to a comma are part of the neighboring fields. (See trimmingRowConfig for the 1.x behavior.)
   */
  val delimiter: Regex
  /**
   * the "string" Regex (see LineParser), for a field which is not quoted. defaults to "[^,\"]*".r,
   * i.e., any characters other than a comma or a quote.
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
 * Default RowConfig trait, which reads CSV as defined by RFC 4180 (https://www.rfc-editor.org/rfc/rfc4180):
 * fields are separated by commas, and spaces are part of a field; a field may be quoted (with "),
 * in which case it may contain commas, line breaks and quotes (which are doubled).
 *
 * TODO Issue #48. Need method for default RowConfig.
 * The problem with the current version is that, if the delimiter is changed, the string regex must change too.
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
   * the delimiter Regex (see LineParser). defaults to ",".r, i.e., a comma, as in RFC 4180:
   * spaces next to a comma are part of the neighboring fields. (See trimmingRowConfig for the 1.x behavior.)
   */
  val delimiter: Regex = """,""".r
  /**
   * the "string" Regex (see LineParser). defaults to "\w+".r, i.e., at least one word character.
   * CONSIDER making the string regex derive from the delimiter
   */
  val string: Regex = """[^,"]*""".r
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
  given defaultRowConfig: DefaultRowConfig with {}

  /**
   * A RowConfig which, like 1.x's default, ignores any spaces next to a comma (so that "a, b" has the fields "a" and "b").
   * This is not RFC 4180, which makes the spaces part of the fields, but it suits data that is written with ", " between fields.
   * To use it, make it the implicit RowConfig, e.g. `implicit val rowConfig: RowConfig = RowConfig.trimmingRowConfig`.
   */
  object trimmingRowConfig extends DefaultRowConfig {
    override val delimiter: Regex = """\s*,\s*""".r
  }

  object defaultEncryptedRowConfig extends DefaultRowConfig {
    override val delimiter: Regex =
      """\|""".r
    override val listSep: Char =
      '&'
    override val string: Regex =
      """[0123456789ABCDEF]+""".r
    override val quote: Char =
      '\''
  }
}
