package com.phasmidsoftware.tableparser.parquet

import java.time.{Instant, LocalDate}
import org.apache.parquet.example.data.simple.SimpleGroup
import org.apache.parquet.io.api.Binary
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
   * @return a Try[T] — Success if the field exists and converts cleanly,
   *         Failure (ParquetParserException) otherwise.
   */
  def convert(group: SimpleGroup, fieldName: String): Try[T]
  // CONSIDER the following definition.
//  def convert(group: SimpleGroup, fieldName: String, pt: PrimitiveType): Try[T]
}

object ParquetCellConverter {
    /**
     * Dispatch to the correct ParquetCellConverter based on the field's runtime type.
     */
    def convertField(group: SimpleGroup, columnName: String, field: java.lang.reflect.Field): Try[Any] =
      field.getType match {
        case c if c == classOf[Boolean]    => BooleanConverter.convert(group, columnName)
        case c if c == classOf[Int]        => IntConverter.convert(group, columnName)
        case c if c == classOf[Long]       => LongConverter.convert(group, columnName)
        case c if c == classOf[Float]      => FloatConverter.convert(group, columnName)
        case c if c == classOf[Double]     => DoubleConverter.convert(group, columnName)
        case c if c == classOf[String]     => StringConverter.convert(group, columnName)
        case c if c == classOf[Instant]    => InstantConverter.convert(group, columnName)
        case c if c == classOf[LocalDate]  => LocalDateConverter.convert(group, columnName)
        case c if c == classOf[BigDecimal] => BigDecimalConverter.convert(group, columnName)
        case other => Failure(ParquetParserException(
          s"No ParquetCellConverter available for type '${other.getName}' " +
                  s"on column '$columnName'"
        ))
      }

    /**
     * Handle OPTIONAL columns -- returns None if the field has no value,
     * Some(value) otherwise.
     * The inner type of Option is determined via generic type reflection.
     */
    def convertOption(group: SimpleGroup, columnName: String, field: java.lang.reflect.Field): Try[Any] = {
      if (group.getFieldRepetitionCount(columnName) == 0)
        Success(None)
      else {
        // Extract the type parameter of Option[T] via generic reflection
        val innerType = field.getGenericType
                .asInstanceOf[java.lang.reflect.ParameterizedType]
                .getActualTypeArguments
                .head
                .asInstanceOf[Class[_]]

        // Synthesize a temporary Field-like dispatch using innerType
        convertByClass(group, columnName, innerType).map(Some(_))
      }
    }

    /**
     * Dispatch by Class[_] directly, used for unwrapped Option inner types.
     */
    def convertByClass(group: SimpleGroup, columnName: String, clazz: Class[_]): Try[Any] =
      clazz match {
        case c if c == classOf[Boolean]    => BooleanConverter.convert(group, columnName)
        case c if c == classOf[Int]        => IntConverter.convert(group, columnName)
        case c if c == classOf[Long]       => LongConverter.convert(group, columnName)
        case c if c == classOf[Float]      => FloatConverter.convert(group, columnName)
        case c if c == classOf[Double]     => DoubleConverter.convert(group, columnName)
        case c if c == classOf[String]     => StringConverter.convert(group, columnName)
        case c if c == classOf[Instant]    => InstantConverter.convert(group, columnName)
        case c if c == classOf[LocalDate]  => LocalDateConverter.convert(group, columnName)
        case c if c == classOf[BigDecimal] => BigDecimalConverter.convert(group, columnName)
        case other => Failure(ParquetParserException(
          s"No ParquetCellConverter available for Option inner type '${other.getName}' " +
                  s"on column '$columnName'"
        ))
      }

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
      Try(Instant.ofEpochMilli(group.getLong(fieldName, 0) / 1000))
    // NOTE: Parquet TIMESTAMP_MICROS stores microseconds since epoch;
    // dividing by 1000 converts to milliseconds for Instant.ofEpochMilli.
    // Use ofEpochSecond with nanos adjustment if microsecond precision matters:
    // val micros = group.getLong(fieldName, 0)
    // Instant.ofEpochSecond(micros / 1_000_000L, (micros % 1_000_000L) * 1000L)
  }

  implicit object LocalDateConverter extends ParquetCellConverter[LocalDate] {
    def convert(group: SimpleGroup, fieldName: String): Try[LocalDate] =
      // Parquet DATE stores days since Unix epoch (1970-01-01) as INT32
      Try(LocalDate.ofEpochDay(group.getInteger(fieldName, 0).toLong))
  }

  implicit object BigDecimalConverter extends ParquetCellConverter[BigDecimal] {
    // NOTE: scale is hardcoded to 0 pending access to PrimitiveType metadata.
    // This converter will produce incorrect results unless the Parquet decimal
    // has scale 0. Revisit when PrimitiveType is passed to convert().
    def convert(group: SimpleGroup, fieldName: String): Try[BigDecimal] =
      Try {
        val binary: Binary = group.getBinary(fieldName, 0)
        BigDecimal(new java.math.BigDecimal(
          new java.math.BigInteger(binary.getBytes), 0
        ))
      }
  }

  /**
   * Converter for optional fields. Handles the case where a Parquet OPTIONAL
   * column contains a null value -- getFieldRepetitionCount returns 0 for nulls.
   */
  implicit def optionConverter[T](implicit
                                  tc: ParquetCellConverter[T]
                                 ): ParquetCellConverter[Option[T]] =
    (group: SimpleGroup, fieldName: String) => Try {
      if (group.getFieldRepetitionCount(fieldName) == 0) None
      else tc.convert(group, fieldName).map(Some(_)).get
    }
}