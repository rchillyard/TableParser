package com.phasmidsoftware.tableparser.core.table

import com.phasmidsoftware.tableparser.core.parse.RowConfig

/**
 * Case class for CSV attributes, especially for rendering as CSV.
 * The defaults (a comma, a double quote and CRLF) write CSV as defined by RFC 4180 (https://www.rfc-editor.org/rfc/rfc4180).
 *
 * For the RowConfig which reads tables as these CsvAttributes write them, see rowConfig
 * (and, the other way round, RowConfig.csvAttributes).
 *
 * @param delimiter the delimiter.
 * @param quote     the quote character.
 * @param newline   the line ending of each record (defaults to CRLF, as in RFC 4180; use "\n" for LF).
 */
case class CsvAttributes(delimiter: String, quote: String, newline: String = "\r\n") {
  /**
   * The RowConfig which reads tables as these CsvAttributes write them (see RowConfig.apply(CsvAttributes)).
   */
  lazy val rowConfig: RowConfig = RowConfig(this)
}

/**
 * Companion object for the `CsvAttributes` case class.
 * Provides utility methods and implicit instances for working with CSV attributes.
 */
object CsvAttributes {
  given defaultCsvAttributes: CsvAttributes = CsvAttributes(",")

  /**
   * Creates a new `CsvAttributes` instance with the specified delimiter and a default quote character.
   *
   * @param delimiter the delimiter to be used in the `CsvAttributes`.
   * @return a new `CsvAttributes` instance with the given delimiter, a double quote as the quote character, and CRLF as the newline.
   */
  def apply(delimiter: String): CsvAttributes = CsvAttributes(delimiter, """"""")
}
