package com.phasmidsoftware.tableparser.core.render

import com.phasmidsoftware.tableparser.core.parse.{ColumnHelper, StringList}
import com.phasmidsoftware.tableparser.core.table.CsvAttributes
import com.phasmidsoftware.tableparser.core.util.Derivation
import java.time.Instant
import org.joda.time.LocalDate
import scala.compiletime.{erasedValue, summonFrom, summonInline}
import scala.deriving.Mirror

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
 * Companion object for the CsvGenerator type class providing implicit instances for various types.
 *
 * The implicit generators defined here are standard implementations used to generate CSV headers
 * for specific data types. These generators are derived from predefined instances in the CsvGenerators object.
 *
 * Implicit instances:
 * - Integer values (`Int`), using `CsvGenerators.CsvGeneratorInt`
 * - String values (`String`), using `CsvGenerators.CsvGeneratorString`
 * - Boolean values (`Boolean`), using `CsvGenerators.CsvGeneratorBoolean`
 * - Double-precision floating-point numbers (`Double`), using `CsvGenerators.CsvGeneratorDouble`
 * - Arbitrarily large integers (`BigInt`), using `CsvGenerators.CsvGeneratorBigInt`
 * - Long integer values (`Long`), using `CsvGenerators.CsvGeneratorLong`
 * - URL values from the `java.net` package (`java.net.URL`), using `CsvGenerators.CsvGeneratorURL`
 */
object CsvGenerator extends CsvGenerators {
  implicit val intGenerator: CsvGenerator[Int] = CsvGenerators.CsvGeneratorInt
  implicit val stringGenerator: CsvGenerator[String] = CsvGenerators.CsvGeneratorString
  implicit val booleanGenerator: CsvGenerator[Boolean] = CsvGenerators.CsvGeneratorBoolean
  implicit val doubleGenerator: CsvGenerator[Double] = CsvGenerators.CsvGeneratorDouble
  implicit val floatGenerator: CsvGenerator[Float] = CsvGenerators.CsvGeneratorFloat
  implicit val shortGenerator: CsvGenerator[Short] = CsvGenerators.CsvGeneratorShort
  implicit val byteGenerator: CsvGenerator[Byte] = CsvGenerators.CsvGeneratorByte
  implicit val bigIntGenerator: CsvGenerator[BigInt] = CsvGenerators.CsvGeneratorBigInt
  implicit val longGenerator: CsvGenerator[Long] = CsvGenerators.CsvGeneratorLong
  implicit val urlGenerator: CsvGenerator[java.net.URL] = CsvGenerators.CsvGeneratorURL
  implicit val localDateGenerator: CsvGenerator[LocalDate] = CsvGenerators.CsvGeneratorLocalDate
  implicit val instantGenerator: CsvGenerator[Instant] = CsvGenerators.CsvGeneratorInstant
  //  implicit val temporalGenerator: CsvGenerator[Temporal] = CsvGenerators.CsvGeneratorTemporal
  implicit val generatorStringList: CsvGenerator[StringList] = sequenceGenerator[String]

  /**
   * Method to derive a CsvGenerator (the column names) for a case class T, so that `case class T(...) derives CsvGenerator` works.
   * This replaces the generator1 ... generator19 methods of 1.x, and has no limit on the number of fields.
   *
   * The column name of each field is exactly the name that CellParser.derived looks for when it reads that field:
   * ColumnHelper[T].lookup of the field's name, given the column name of the enclosing field (if any).
   * So a table which is written can be read back. For example, if ColumnHelper[Principal] has the prefix "$x_$c",
   * the name field of a Principal field called director is in the column director_name.
   * Without a ColumnHelper of its own, a field's column name is its name.
   * A field whose type is itself a case class, and which has no CsvGenerator of its own, is derived in turn.
   *
   * NOTE: for a case class defined inside a method, see the NOTE on CellParser.derived.
   *
   * @param m  the Mirror of T.
   * @param ca the CsvAttributes.
   * @tparam T a case class.
   * @return a CsvProductGenerator[T].
   */
  inline def derived[T](using m: Mirror.ProductOf[T], ca: CsvAttributes): CsvProductGenerator[T] =
    new ProductCsvGenerator[T](Derivation.fieldNames[m.MirroredElemLabels], fieldGenerators[m.MirroredElemTypes])(using ca, summonInline[ColumnHelper[T]])

  private[render] inline def fieldGenerators[Ts <: Tuple](using CsvAttributes): List[CsvGenerator[?]] = inline erasedValue[Ts] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => fieldGenerator[t] :: fieldGenerators[ts]
  }

  // Prefer a field type's own CsvGenerator; otherwise, if it is a case class, derive one.
  private inline def fieldGenerator[A](using CsvAttributes): CsvGenerator[A] = summonFrom {
    case g: CsvGenerator[A] => g
    case m: Mirror.ProductOf[A] => derived[A](using m, summonInline[CsvAttributes])
  }
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
class StandardCsvGenerator[T](using ca: CsvAttributes) extends CsvGenerator[T] {
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
    CsvRenderers.quoteIfNecessary((po map (w => s"$w.")).getOrElse("") + name)(using ca)

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
abstract class BaseCsvProductGenerator[T](using ca: CsvAttributes) extends StandardCsvGenerator[T] with CsvProductGenerator[T]

/**
 * The CsvGenerator of a case class T, as derived by CsvGenerator.derived.
 *
 * @param fieldNames      the names of the fields of T, in declaration order.
 * @param fieldGenerators the CsvGenerators of the fields of T, in the same order.
 * @param ca              the CsvAttributes.
 * @param helper          the ColumnHelper[T], which gives the column name of each field.
 * @tparam T the type of the case class.
 */
class ProductCsvGenerator[T](fieldNames: List[String], fieldGenerators: List[CsvGenerator[?]])(using ca: CsvAttributes, helper: ColumnHelper[T]) extends BaseCsvProductGenerator[T] {
  /**
   * Method to generate the column names of the fields of T.
   * The name of the enclosing field (if any) is no (or po, if that is all there is):
   * each field's column name is then helper.lookup(that name, field name), as in CellParser.derived.
   * A field which is itself a case class is given its column name as the name of its enclosing field.
   */
  def toColumnNames(po: Option[String], no: Option[String]): String = {
    val wo = merge(po, no)
    (fieldNames zip fieldGenerators) map ((name, generator) => generator.toColumnName(None, helper.lookup(wo, name))) mkString ca.delimiter
  }
}
