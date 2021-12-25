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
    * @param po the (optional) name of the parent.
    * @param no the (optional) name of this column.
    * @return a list of names of the form parent.column.
    */
  def toColumnNames(to: Option[T], po: Option[String], no: Option[String]): String
}

class BaseCsvGenerator[T](implicit ca: CsvAttributes) extends CsvGenerator[T] {
  val csvAttributes: CsvAttributes = ca

  def toColumnNames(to: Option[T], po: Option[String], no: Option[String]): String = (po map (w => s"$w.")).getOrElse("") + no.getOrElse("")
}
