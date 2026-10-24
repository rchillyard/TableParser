/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.table.CsvAttributes
import scala.util.matching.Regex

/**
 * Case class to define the configuration for parsing a row (see LineParser).
 *
 * The default reads CSV as defined by RFC 4180 (https://www.rfc-editor.org/rfc/rfc4180):
 * fields are separated by commas, and spaces are part of a field; a field may be quoted (with "),
 * in which case it may contain commas, line breaks and quotes (which are doubled).
 *
 * The regular expressions which LineParser uses are derived from the characters,
 * so that changing the delimiter (for example) changes everything that depends on it.
 * Use the copy methods (withDelimiter, etc.) to change one property, e.g.
 * `given RowConfig = RowConfig.default.withDelimiter('\t')` or simply `given RowConfig = RowConfig(delimiter = '\t')`.
 *
 * @param delimiter     the character which separates fields. Defaults to ','.
 * @param quote         the character which encloses a quoted field. Defaults to '"'.
 * @param listSep       the character which separates the elements of a list within a field. Defaults to '|'.
 * @param listEnclosure the (two) characters which enclose a list within a field, or "" if lists are not enclosed. Defaults to "{}".
 * @param trim          if true, any spaces next to a delimiter are ignored (so that "a, b" has the fields "a" and "b"),
 *                      as in 1.x; this is not RFC 4180. Defaults to false.
 * @param maybeString   an explicit Regex for a field which is not quoted, if the default
 *                      (any characters other than the delimiter and the quote) is not wanted.
 */
case class RowConfig(delimiter: Char = ',', quote: Char = '"', listSep: Char = '|', listEnclosure: String = "{}", trim: Boolean = false, maybeString: Option[Regex] = None):

  /**
   * The Regex which matches a delimiter (with any spaces around it, if trim is true).
   */
  lazy val delimiterRegex: Regex =
    val d = Regex.quote(delimiter.toString)
    (if (trim) s"""\\s*$d\\s*""" else d).r

  /**
   * The Regex which matches a field which is not quoted: maybeString if it is defined;
   * otherwise, any characters other than the delimiter and the quote
   * (if trim is true, not ending with a space, which belongs to the following delimiter).
   */
  lazy val string: Regex = maybeString getOrElse:
    val others = s"${RowConfig.inClass(delimiter)}${RowConfig.inClass(quote)}"
    (if (trim) s"""(?:[^$others]*[^$others\\s])?""" else s"[^$others]*").r

  /**
   * The CsvAttributes for writing tables as this RowConfig reads them.
   * If trim is true, the delimiter is followed by a space.
   */
  lazy val csvAttributes: CsvAttributes =
    CsvAttributes(if (trim) s"$delimiter " else delimiter.toString, quote.toString)

  infix def withDelimiter(c: Char): RowConfig = copy(delimiter = c)

  infix def withQuote(c: Char): RowConfig = copy(quote = c)

  infix def withListSep(c: Char): RowConfig = copy(listSep = c)

  infix def withListEnclosure(w: String): RowConfig = copy(listEnclosure = w)

  infix def withTrim(b: Boolean): RowConfig = copy(trim = b)

  infix def withString(r: Regex): RowConfig = copy(maybeString = Some(r))

  override def toString: String = s"RowConfig: delimiter='$delimiter', string='$string', listSep='$listSep', listEnclosure='$listEnclosure', quote='$quote', trim=$trim"

/**
 * Companion object to RowConfig.
 */
object RowConfig:

  /**
   * The default RowConfig: RFC 4180.
   */
  val default: RowConfig = RowConfig()

  // CONSIDER removing this default row configuration. It might be better to have the compiler issue a warning when it's missing.
  given defaultRowConfig: RowConfig = default

  /**
   * A RowConfig which, like 1.x's default, ignores any spaces next to a comma (so that "a, b" has the fields "a" and "b").
   * This is not RFC 4180, which makes the spaces part of the fields, but it suits data that is written with ", " between fields.
   * To use it, make it the given RowConfig, e.g. `given RowConfig = RowConfig.trimmingRowConfig`.
   */
  val trimmingRowConfig: RowConfig = default withTrim true

  /**
   * The RowConfig of the rows of an encrypted table: fields separated by '|', each a hex string.
   */
  val defaultEncryptedRowConfig: RowConfig =
    RowConfig(delimiter = '|', quote = '\'', listSep = '&', maybeString = Some("""[0123456789ABCDEF]+""".r))

  /**
   * Method to get the RowConfig which reads tables as the given CsvAttributes writes them.
   *
   * @param csvAttributes the CsvAttributes, whose delimiter (ignoring any spaces) and quote must each be one character.
   * @return a RowConfig, which trims spaces if the delimiter of csvAttributes has any.
   */
  def apply(csvAttributes: CsvAttributes): RowConfig =
    val w = csvAttributes.delimiter
    val d = w.filter(_ != ' ') match
      case "" => w // the delimiter is a space
      case x => x
    require(d.length == 1 && csvAttributes.quote.length == 1, s"RowConfig: cannot read tables written with $csvAttributes")
    RowConfig(delimiter = d.head, quote = csvAttributes.quote.head, trim = d != w)

  // A character as it must appear inside a character class of a Regex.
  private def inClass(c: Char): String = if ("""\^]-[""".contains(c)) s"\\$c" else c.toString
