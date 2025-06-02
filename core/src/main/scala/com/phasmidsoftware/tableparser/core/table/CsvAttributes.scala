package com.phasmidsoftware.tableparser.core.table

/**
 * Case class for CSV attributes, especially for rendering as CSV.
 *
 * CONSIDER merging this with RowConfig.
 *
 * @param delimiter the delimiter.
 * @param quote     the quote character.
 */
case class CsvAttributes(delimiter: String, quote: String)

/**
 * Companion object for the `CsvAttributes` case class.
 * Provides utility methods and implicit instances for working with CSV attributes.
 */
object CsvAttributes {
  implicit val defaultCsvAttributes: CsvAttributes = CsvAttributes(",")

  /**
   * Creates a new `CsvAttributes` instance with the specified delimiter and a default quote character.
   *
   * @param delimiter the delimiter to be used in the `CsvAttributes`.
   * @return a new `CsvAttributes` instance with the given delimiter and an empty string as the quote character.
   */
  def apply(delimiter: String): CsvAttributes = CsvAttributes(delimiter, """"""")
}
