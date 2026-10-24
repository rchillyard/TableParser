package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper
import com.phasmidsoftware.tableparser.core.util.Derivation
import java.time.{Instant, LocalDate}
import org.apache.parquet.example.data.simple.SimpleGroup
import scala.compiletime.{erasedValue, summonFrom, summonInline}
import scala.deriving.Mirror
import scala.util.{Failure, Success, Try}

/**
 * Type class for converting a field value from a Parquet SimpleGroup
 * directly to a Scala type T, without going through String.
 *
 * @tparam T the target Scala type.
 */
trait ParquetCellConverter[T] {
  /**
   * Extract and convert the value of the named field from the given SimpleGroup.
   *
   * @param group     the SimpleGroup representing one Parquet row.
   * @param fieldName the Parquet column name (a converter for a case class ignores it, and reads its own columns).
   * @return a Try[T].
   */
  def convert(group: SimpleGroup, fieldName: String): Try[T]

  /**
   * The Parquet columns which this converter reads (used to validate the schema before any rows are read).
   * By default, it reads just the one column, of any supported type.
   *
   * @param field      the name of the parameter (for error messages).
   * @param columnName the name of the Parquet column.
   * @return the columns which this converter reads.
   */
  def columns(field: String, columnName: String): Seq[ParquetColumn] =
    Seq(ParquetColumn(field, columnName, None, optional = false))
}

/**
 * A Parquet column which is read by a ParquetCellConverter.
 *
 * @param field      the name of the parameter which the column is read into.
 * @param columnName the name of the column in the Parquet schema.
 * @param maybeClass the Scala type which the column must map to (see ParquetTypeMapper), if any.
 * @param optional   true if the parameter is an Option (so the column may be OPTIONAL).
 */
case class ParquetColumn(field: String, columnName: String, maybeClass: Option[Class[?]], optional: Boolean)

object ParquetCellConverter {

  /**
   * Method to create a ParquetCellConverter for a primitive column.
   *
   * @param clazz the Scala type which the column must map to (see ParquetTypeMapper).
   * @param f     a function which gets the value of the named column from a SimpleGroup.
   * @tparam T the type of the result.
   * @return a ParquetCellConverter[T].
   */
  def primitive[T](clazz: Class[?])(f: (SimpleGroup, String) => T): ParquetCellConverter[T] = new ParquetCellConverter[T] {
    def convert(group: SimpleGroup, fieldName: String): Try[T] = Try(f(group, fieldName))

    override def columns(field: String, columnName: String): Seq[ParquetColumn] =
      Seq(ParquetColumn(field, columnName, Some(clazz), optional = false))
  }

  given BooleanConverter: ParquetCellConverter[Boolean] = primitive(classOf[Boolean])(_.getBoolean(_, 0))

  given IntConverter: ParquetCellConverter[Int] = primitive(classOf[Int])(_.getInteger(_, 0))

  given LongConverter: ParquetCellConverter[Long] = primitive(classOf[Long])(_.getLong(_, 0))

  given FloatConverter: ParquetCellConverter[Float] = primitive(classOf[Float])(_.getFloat(_, 0))

  given DoubleConverter: ParquetCellConverter[Double] = primitive(classOf[Double])(_.getDouble(_, 0))

  given StringConverter: ParquetCellConverter[String] = primitive(classOf[String])(_.getBinary(_, 0).toStringUsingUTF8)

  given InstantConverter: ParquetCellConverter[Instant] = primitive(classOf[Instant]) { (group, fieldName) =>
    val micros = group.getLong(fieldName, 0)
    Instant.ofEpochSecond(micros / 1_000_000L, (micros % 1_000_000L) * 1000L)
  }

  given LocalDateConverter: ParquetCellConverter[LocalDate] =
    primitive(classOf[LocalDate])((group, fieldName) => LocalDate.ofEpochDay(group.getInteger(fieldName, 0).toLong))

  // NOTE: scale hardcoded to 0 pending access to PrimitiveType metadata.
  given BigDecimalConverter: ParquetCellConverter[BigDecimal] = primitive(classOf[BigDecimal]) { (group, fieldName) =>
    val binary = group.getBinary(fieldName, 0)
    BigDecimal(new java.math.BigDecimal(new java.math.BigInteger(binary.getBytes), 0))
  }

  // ── Option converter ────────────────────────────────────────────────────────

  /**
   * Converter for Option[T] fields.
   * Returns None if the field has no value in this row, Some(value) otherwise.
   * T must be read from a single column (an optional case class is not supported).
   */
  given optionConverter[T: ParquetCellConverter]: ParquetCellConverter[Option[T]] =
    new ParquetCellConverter[Option[T]] {
      def convert(group: SimpleGroup, fieldName: String): Try[Option[T]] =
        if (group.getFieldRepetitionCount(fieldName) == 0)
          Success(None)
        else
          summon[ParquetCellConverter[T]].convert(group, fieldName).map(Some(_))

      override def columns(field: String, columnName: String): Seq[ParquetColumn] =
        summon[ParquetCellConverter[T]].columns(field, columnName).map(_.copy(optional = true))
    }

  // ── Derivation for case classes ─────────────────────────────────────────────

  /**
   * Method to derive a ParquetCellConverter[T] for a case class T, so that `case class T(...) derives ParquetCellConverter` works.
   * This replaces the converter2 ... converter8 methods (and the reflection) of 1.x, and has no limit on the number of fields.
   *
   * Each field is read from the column whose name is given by ColumnHelper[T] for that field's name.
   * The field names come from the Mirror (at compile time), in declaration order.
   * A field whose type is itself a case class (a "grouped" field), and which has no ParquetCellConverter of its own,
   * is derived in turn: its fields are read from the same (flat) row, using its own ColumnHelper.
   *
   * NOTE: as for CellParser.derived, don't derive the converter of a case class defined inside a method
   * in a companion object that you write for it.
   *
   * @param m the Mirror of T.
   * @tparam T a case class.
   * @return a ParquetCellConverter[T].
   */
  inline def derived[T](using m: Mirror.ProductOf[T]): ParquetCellConverter[T] =
    new ProductParquetCellConverter[T](
      Derivation.fieldNames[m.MirroredElemLabels],
      fieldConverters[m.MirroredElemTypes],
      xs => m.fromProduct(Tuple.fromArray(xs.toArray))
    )(using summonInline[ColumnHelper[T]])

  /**
   * Method to get the ParquetCellConverter of T: its own, if there is one; otherwise, if T is a case class, a derived one.
   *
   * @tparam T the type of the result.
   * @return a ParquetCellConverter[T].
   */
  inline def apply[T]: ParquetCellConverter[T] = summonFrom {
    case c: ParquetCellConverter[T] => c
    case m: Mirror.ProductOf[T] => derived[T](using m)
  }

  private inline def fieldConverters[Ts <: Tuple]: List[ParquetCellConverter[?]] = inline erasedValue[Ts] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => apply[t] :: fieldConverters[ts]
  }
}

/**
 * The ParquetCellConverter of a case class T, as derived by ParquetCellConverter.derived.
 *
 * @param fieldNames the names of the fields of T, in declaration order.
 * @param converters the ParquetCellConverters of the fields of T, in the same order.
 * @param construct  a function to construct a T from the values of its fields.
 * @param helper     the ColumnHelper[T] which maps field names to column names.
 * @tparam T the type of the result.
 */
class ProductParquetCellConverter[T](fieldNames: List[String], converters: List[ParquetCellConverter[?]], construct: Seq[Any] => T)(using helper: ColumnHelper[T]) extends ParquetCellConverter[T] {

  private val fields: List[(String, String, ParquetCellConverter[?])] =
    fieldNames.zip(converters).map((f, c) => (f, helper.lookup(None, f), c))

  /**
   * Method to convert a row (or part of a row) into a T.
   *
   * @param group     the SimpleGroup representing one Parquet row.
   * @param fieldName ignored: each field of T is read from its own column.
   * @return a Try[T].
   */
  def convert(group: SimpleGroup, fieldName: String): Try[T] = Try {
    val values = fields map { (f, columnName, c) =>
      c.convert(group, columnName) match {
        case Success(v) => v
        case Failure(e: ParquetParserException) => throw e
        case Failure(e) => throw ParquetParserException(
          s"Failed to convert field '$f' from column '$columnName': ${e.getClass.getSimpleName}: ${e.getMessage}",
          Some(e)
        )
      }
    }
    construct(values)
  }

  override def columns(field: String, columnName: String): Seq[ParquetColumn] =
    fields flatMap ((f, w, c) => c.columns(f, w))
}
