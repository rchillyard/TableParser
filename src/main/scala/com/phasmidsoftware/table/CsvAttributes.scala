package com.phasmidsoftware.table

case class CsvAttributes(delimiter: String, quote: String)

object CsvAttributes {
  implicit val defaultCsvAttributes: CsvAttributes = CsvAttributes(",")

  def apply(delimiter: String): CsvAttributes = CsvAttributes(delimiter, """"""")
}


/**
  * Type class for rendering headers to CSV.
  *
  * @tparam T the type of the objects to be rendered by CSV.
  */
trait CsvGenerator[T] {
  // CONSIDER removing this abstract val.
  val csvAttributes: CsvAttributes

  /**
    * Method to generate a list of appropriate column names for a value of t.
    *
    * CONSIDER is "to" actually necessary?
    *
    * @param to an optional T value (ignored).
    * @param wo the name of the parent.
    * @return a list of names of the form parent.column.
    */
  def toColumnNames(to: Option[T], wo: Option[String], name: String): String
}
