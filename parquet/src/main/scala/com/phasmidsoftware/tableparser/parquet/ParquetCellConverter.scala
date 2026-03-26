package com.phasmidsoftware.tableparser.parquet

import java.time.{Instant, LocalDate}
import org.apache.parquet.example.data.simple.SimpleGroup
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
   * @param fieldName the Parquet column name.
   * @return a Try[T].
   */
  def convert(group: SimpleGroup, fieldName: String): Try[T]
}

object ParquetCellConverter {

  implicit object BooleanConverter extends ParquetCellConverter[Boolean] {
    def convert(group: SimpleGroup, fieldName: String): Try[Boolean] =
      Try(group.getBoolean(fieldName, 0))
  }

  implicit object IntConverter extends ParquetCellConverter[Int] {
    def convert(group: SimpleGroup, fieldName: String): Try[Int] =
      Try(group.getInteger(fieldName, 0))
  }

  implicit object LongConverter extends ParquetCellConverter[Long] {
    def convert(group: SimpleGroup, fieldName: String): Try[Long] =
      Try(group.getLong(fieldName, 0))
  }

  implicit object FloatConverter extends ParquetCellConverter[Float] {
    def convert(group: SimpleGroup, fieldName: String): Try[Float] =
      Try(group.getFloat(fieldName, 0))
  }

  implicit object DoubleConverter extends ParquetCellConverter[Double] {
    def convert(group: SimpleGroup, fieldName: String): Try[Double] =
      Try(group.getDouble(fieldName, 0))
  }

  implicit object StringConverter extends ParquetCellConverter[String] {
    def convert(group: SimpleGroup, fieldName: String): Try[String] =
      Try(group.getBinary(fieldName, 0).toStringUsingUTF8)
  }

  implicit object InstantConverter extends ParquetCellConverter[Instant] {
    def convert(group: SimpleGroup, fieldName: String): Try[Instant] =
      Try {
        val micros = group.getLong(fieldName, 0)
        // TIMESTAMP_MICROS: microseconds since epoch
        Instant.ofEpochSecond(micros / 1_000_000L, (micros % 1_000_000L) * 1000L)
      }
  }

  implicit object LocalDateConverter extends ParquetCellConverter[LocalDate] {
    def convert(group: SimpleGroup, fieldName: String): Try[LocalDate] =
      // DATE: days since Unix epoch (1970-01-01) stored as INT32
      Try(LocalDate.ofEpochDay(group.getInteger(fieldName, 0).toLong))
  }

  implicit object BigDecimalConverter extends ParquetCellConverter[BigDecimal] {
    // NOTE: scale hardcoded to 0 pending access to PrimitiveType metadata.
    // Will produce incorrect results unless Parquet decimal has scale 0.
    def convert(group: SimpleGroup, fieldName: String): Try[BigDecimal] =
      Try {
        val binary = group.getBinary(fieldName, 0)
        BigDecimal(new java.math.BigDecimal(
          new java.math.BigInteger(binary.getBytes), 0
        ))
      }
  }

  /**
   * Dispatch to the correct converter based on the field's runtime type.
   * Used by ParquetRowParser for non-optional fields.
   */
  def convertField(
                          group: SimpleGroup,
                          columnName: String,
                          field: java.lang.reflect.Field
                  ): Try[Any] =
    convertByClass(group, columnName, field.getType)

  /**
   * Handle OPTIONAL columns.
   * Returns None if the field has no value in this row, Some(value) otherwise.
   * The inner type of Option[T] is determined via generic type reflection.
   */
  def convertOption(
                           group: SimpleGroup,
                           columnName: String,
                           parquetType: org.apache.parquet.schema.PrimitiveType
                   ): Try[Any] =
    if (group.getFieldRepetitionCount(columnName) == 0)
      Success(None)
    else
      ParquetTypeMapper.mapType(parquetType) match {
        case Left(ex) => Failure(ex)
        case Right(clazz) => convertByClass(group, columnName, clazz).map(Some(_))
      }

  /**
   * Dispatch to the correct converter by Class[_] directly.
   * Used for both non-optional fields and Option inner types.
   */
  def convertByClass(
                            group: SimpleGroup,
                            columnName: String,
                            clazz: Class[_]
                    ): Try[Any] =
    clazz match {
      case c if c == classOf[Boolean] || c == classOf[java.lang.Boolean] => BooleanConverter.convert(group, columnName)
      case c if c == classOf[Int] || c == classOf[java.lang.Integer] => IntConverter.convert(group, columnName)
      case c if c == classOf[Long] || c == classOf[java.lang.Long] => LongConverter.convert(group, columnName)
      case c if c == classOf[Float] || c == classOf[java.lang.Float] => FloatConverter.convert(group, columnName)
      case c if c == classOf[Double] || c == classOf[java.lang.Double] => DoubleConverter.convert(group, columnName)
      case c if c == classOf[String] => StringConverter.convert(group, columnName)
      case c if c == classOf[Instant] => InstantConverter.convert(group, columnName)
      case c if c == classOf[LocalDate] => LocalDateConverter.convert(group, columnName)
      case c if c == classOf[BigDecimal] || c == classOf[java.math.BigDecimal] => BigDecimalConverter.convert(group, columnName)
      case other =>
        Failure(ParquetParserException(
          s"No ParquetCellConverter available for type '${other.getName}' " +
                  s"on column '$columnName'"
        ))
    }
}