package com.phasmidsoftware.tableparser.core.render

import com.phasmidsoftware.tableparser.core.table.CsvAttributes
import com.phasmidsoftware.tableparser.core.util.Reflection
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
  /**
   * Extracts the names of the fields for the type `T` using runtime reflection.
   * This method is useful for generating field names for case classes or other types
   * in the context of CSV generation.
   *
   * @param tc An implicit `ClassTag` for the type `T`, which is used to access
   *           runtime type information about the type.
   * @return An array of strings, where each string corresponds to the name of a field in the type `T`.
   */
  def fieldNames(implicit tc: ClassTag[T]): Array[String] =
    Reflection.extractFieldNames(tc)

  /**
   * Method to generate a list of appropriate column names for a value of t.
   *
   * @param po the (optional) name of the parent.
   * @param no the (optional) name of this column.
   * @return a list of names of the form parent.column.
   */
  def toColumnNames(po: Option[String], no: Option[String]): String

  /**
   * Converts the given optional parent name and column name into a single formatted column name.
   *
   * @param po   An optional string representing the parent column or prefix, which may be used
   *             to create a nested or hierarchical column name.
   * @param name The name of the base column to be formatted.
   * @return A formatted string representing the column name, typically following the pattern
   *         "parent.column", if a parent name is provided.
   */
  override def toColumnName(po: Option[String], name: String): String =
    toColumnNames(po, Some(name))
}

/**
 * The `StandardCsvGenerator` class extends the `CsvGenerator` trait to provide functionality
 * for generating standardized CSV output. It utilizes implicit `CsvAttributes` for configuring
 * the delimiter and quote character used in the CSV representation.
 *
 * @tparam T the type of the objects for which this generator creates column names.
 * @constructor
 * Creates a new `StandardCsvGenerator` instance with implicit `CsvAttributes` providing
 * the configuration for CSV generation.
 * @param ca an implicit instance of `CsvAttributes` which defines the delimiter and quote character.
 */
class StandardCsvGenerator[T](implicit ca: CsvAttributes) extends CsvGenerator[T] {
  val csvAttributes: CsvAttributes = ca

  /**
   * Converts a column name into a qualified column name by optionally prefixing it with
   * a provided qualifier.
   *
   * @param po   an optional qualifier (e.g., a table alias) to prepend to the column name.
   *             If `None`, no qualifier is prepended.
   * @param name the column name to be qualified.
   * @return a fully-qualified column name in the format "qualifier.name" if `po` is defined,
   *         or just `name` if `po` is `None`.
   */
  def toColumnName(po: Option[String], name: String): String =
    (po map (w => s"$w.")).getOrElse("") + name

  /**
   * Merges two optional strings into a single optional string.
   * If both `po` and `no` are defined, the resulting string is their concatenation in the format `po.no`.
   * If only one of the options is defined, it returns that single option.
   * If neither are defined, it returns `None`.
   *
   * @param po an optional prefix string; if defined, it will form the first part of the resulting string.
   * @param no an optional suffix string; if defined, it will form the second part of the resulting string.
   * @return an optional string in the format `po.no` if both options are defined,
   *         or the defined option if only one is present, or `None` if both are `None`.
   */
  def merge(po: Option[String], no: Option[String]): Option[String] = po match {
    case Some(p) =>
      no match {
        case Some(n) =>
          Some(s"$p.$n")
        case None =>
          po
      }
    case None =>
      no
  }
}

/**
 * Base class for generating CSV output for product types, such as case classes,
 * by extending both `StandardCsvGenerator` and `CsvProductGenerator`.
 *
 * This abstract class provides a common foundation for implementing CSV generation
 * for specific types of products by combining the column name handling from `StandardCsvGenerator`
 * with the field name extraction capabilities of `CsvProductGenerator`.
 *
 * @tparam T the type of the product for which this generator is responsible.
 * @param ca an implicit parameter providing an instance of `CsvAttributes`, which defines
 *           the configuration for the CSV generation, such as the delimiter and quote character.
 * @see `CsvAttributes` for CSV configuration options.
 * @see `StandardCsvGenerator` for handling standard column name generation.
 * @see `CsvProductGenerator` for field name extraction specific to product types.
 */
abstract class BaseCsvProductGenerator[T](implicit ca: CsvAttributes) extends StandardCsvGenerator[T] with CsvProductGenerator[T]