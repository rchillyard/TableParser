package com.phasmidsoftware.table

/**
 * Case class for CSV attributes, especially for rendering as CSV.
 *
 * CONSIDER merging this with RowConfig.
 *
 * @param delimiter the delimiter.
 * @param quote     the quote character.
 */
case class CsvAttributes(delimiter: String, quote: String)

object CsvAttributes {
  implicit val defaultCsvAttributes: CsvAttributes = CsvAttributes(",")

  def apply(delimiter: String): CsvAttributes = CsvAttributes(delimiter, """"""")
}
