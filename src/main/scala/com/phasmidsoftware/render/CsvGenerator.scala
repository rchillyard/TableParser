package com.phasmidsoftware.render

import com.phasmidsoftware.table.CsvAttributes
import com.phasmidsoftware.util.Reflection
import scala.reflect.ClassTag


/**
 * Trait (typeclass) for generating headers for CSV output.
 *
 * CONSIDER why do we need this? It takes a lot of effort to set up, which is essentially the same as for CsvRenderer[T].
 *
 * NOTE: this is an unusual type class in that none of its methods reference type T.
 *
 * @tparam T the contravariant type of the objects to be rendered by CSV.
 */
trait CsvGenerator[-T] {
  // CONSIDER removing this abstract val.
  val csvAttributes: CsvAttributes

  /**
   * Method to generate a list of appropriate column names for a value of t.
   *
   * @param po   the (optional) name of the parent.
   * @param name the name of this column.
   * @return a list of names of the form parent.column.
   */
  def toColumnName(po: Option[String], name: String): String
}

/**
 * Trait (typeclass) for generating headers for CSV output.
 *
 * NOTE: this is an unusual type class in that none of its methods reference type T.
 *
 * @tparam T the type of the objects to be rendered by CSV.
 */
trait CsvProductGenerator[T] extends CsvGenerator[T] {
  def fieldNames(implicit tc: ClassTag[T]): Array[String] = Reflection.extractFieldNames(tc)

  /**
   * Method to generate a list of appropriate column names for a value of t.
   *
   * @param po the (optional) name of the parent.
   * @param no the (optional) name of this column.
   * @return a list of names of the form parent.column.
   */
  def toColumnNames(po: Option[String], no: Option[String]): String

  override def toColumnName(po: Option[String], name: String): String = toColumnNames(po, Some(name))
}

class StandardCsvGenerator[T](implicit ca: CsvAttributes) extends CsvGenerator[T] {
  val csvAttributes: CsvAttributes = ca

  def toColumnName(po: Option[String], name: String): String = (po map (w => s"$w.")).getOrElse("") + name

  def merge(po: Option[String], no: Option[String]): Option[String] = po match {
    case Some(p) =>
      no match {
        case Some(n) => Some(s"$p.$n")
        case None => po
      }
    case None => no
  }
}

abstract class BaseCsvProductGenerator[T](implicit ca: CsvAttributes) extends StandardCsvGenerator[T] with CsvProductGenerator[T]