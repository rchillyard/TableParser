package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper
import com.phasmidsoftware.tableparser.core.util.Reflection
import java.time.{Instant, LocalDate}
import org.apache.parquet.example.data.simple.SimpleGroup
import scala.reflect.ClassTag
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
        Instant.ofEpochSecond(micros / 1_000_000L, (micros % 1_000_000L) * 1000L)
      }
  }

  implicit object LocalDateConverter extends ParquetCellConverter[LocalDate] {
    def convert(group: SimpleGroup, fieldName: String): Try[LocalDate] =
      Try(LocalDate.ofEpochDay(group.getInteger(fieldName, 0).toLong))
  }

  implicit object BigDecimalConverter extends ParquetCellConverter[BigDecimal] {
    // NOTE: scale hardcoded to 0 pending access to PrimitiveType metadata.
    def convert(group: SimpleGroup, fieldName: String): Try[BigDecimal] =
      Try {
        val binary = group.getBinary(fieldName, 0)
        BigDecimal(new java.math.BigDecimal(
          new java.math.BigInteger(binary.getBytes), 0
        ))
      }
  }

  // ── Option converter ────────────────────────────────────────────────────────

  /**
   * Converter for Option[T] fields.
   * Returns None if the field has no value in this row, Some(value) otherwise.
   */
  implicit def optionConverter[T: ParquetCellConverter]: ParquetCellConverter[Option[T]] =
    new ParquetCellConverter[Option[T]] {
      def convert(group: SimpleGroup, fieldName: String): Try[Option[T]] =
        if (group.getFieldRepetitionCount(fieldName) == 0)
          Success(None)
        else
          implicitly[ParquetCellConverter[T]].convert(group, fieldName).map(Some(_))
    }

  // ── converterN factory methods ──────────────────────────────────────────────

  /**
   * Returns the effective field names for type T: uses the provided list if non-empty,
   * otherwise reflects on the ClassTag. Mirrors CellParsers.fieldNames.
   */
  private def effectiveFieldNames[T: ClassTag](fields: Seq[String]): Array[String] =
    if (fields.isEmpty) Reflection.extractFieldNames(implicitly[ClassTag[T]])
    else fields.toArray

  /**
   * Method to return a ParquetCellConverter[T] where T is a 2-ary Product,
   * reading its fields from the same flat SimpleGroup by name via ColumnHelper[T].
   *
   * @param construct a function (P1, P2) => T, usually the apply method of a case class.
   * @param fields    optional explicit field names; if Nil, field names are obtained by reflection.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam T  the target case class type.
   * @return a ParquetCellConverter[T].
   */
  def converter2[P1: ParquetCellConverter, P2: ParquetCellConverter, T <: Product : ClassTag : ColumnHelper]
  (construct: (P1, P2) => T, fields: Seq[String] = Nil): ParquetCellConverter[T] =
    new ParquetCellConverter[T] {
      private val fs = effectiveFieldNames[T](fields)
      private val f1 = fs(0)
      private val f2 = fs(1)

      def convert(group: SimpleGroup, fieldName: String): Try[T] = {
        val h = implicitly[ColumnHelper[T]]
        for {
          p1 <- implicitly[ParquetCellConverter[P1]].convert(group, h.lookup(None, f1))
          p2 <- implicitly[ParquetCellConverter[P2]].convert(group, h.lookup(None, f2))
        } yield construct(p1, p2)
      }
    }

  /**
   * Method to return a ParquetCellConverter[T] where T is a 3-ary Product.
   *
   * @param construct a function (P1, P2, P3) => T, usually the apply method of a case class.
   * @param fields    optional explicit field names; if Nil, field names are obtained by reflection.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam T  the target case class type.
   * @return a ParquetCellConverter[T].
   */
  def converter3[P1: ParquetCellConverter, P2: ParquetCellConverter, P3: ParquetCellConverter, T <: Product : ClassTag : ColumnHelper]
  (construct: (P1, P2, P3) => T, fields: Seq[String] = Nil): ParquetCellConverter[T] =
    new ParquetCellConverter[T] {
      private val fs = effectiveFieldNames[T](fields)
      private val f1 = fs(0)

      def convert(group: SimpleGroup, fieldName: String): Try[T] = {
        val h = implicitly[ColumnHelper[T]]
        for {
          p1 <- implicitly[ParquetCellConverter[P1]].convert(group, h.lookup(None, f1))
          t <- converter2[P2, P3, T](construct(p1, _, _), fs.tail.toSeq).convert(group, fieldName)
        } yield t
      }
    }

  /**
   * Method to return a ParquetCellConverter[T] where T is a 4-ary Product.
   *
   * @param construct a function (P1, P2, P3, P4) => T, usually the apply method of a case class.
   * @param fields    optional explicit field names; if Nil, field names are obtained by reflection.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam T  the target case class type.
   * @return a ParquetCellConverter[T].
   */
  def converter4[P1: ParquetCellConverter, P2: ParquetCellConverter, P3: ParquetCellConverter, P4: ParquetCellConverter, T <: Product : ClassTag : ColumnHelper]
  (construct: (P1, P2, P3, P4) => T, fields: Seq[String] = Nil): ParquetCellConverter[T] =
    new ParquetCellConverter[T] {
      private val fs = effectiveFieldNames[T](fields)
      private val f1 = fs(0)

      def convert(group: SimpleGroup, fieldName: String): Try[T] = {
        val h = implicitly[ColumnHelper[T]]
        for {
          p1 <- implicitly[ParquetCellConverter[P1]].convert(group, h.lookup(None, f1))
          t <- converter3[P2, P3, P4, T](construct(p1, _, _, _), fs.tail.toSeq).convert(group, fieldName)
        } yield t
      }
    }

  /**
   * Method to return a ParquetCellConverter[T] where T is a 5-ary Product.
   *
   * @param construct a function (P1, P2, P3, P4, P5) => T, usually the apply method of a case class.
   * @param fields    optional explicit field names; if Nil, field names are obtained by reflection.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam T  the target case class type.
   * @return a ParquetCellConverter[T].
   */
  def converter5[P1: ParquetCellConverter, P2: ParquetCellConverter, P3: ParquetCellConverter, P4: ParquetCellConverter, P5: ParquetCellConverter, T <: Product : ClassTag : ColumnHelper]
  (construct: (P1, P2, P3, P4, P5) => T, fields: Seq[String] = Nil): ParquetCellConverter[T] =
    new ParquetCellConverter[T] {
      private val fs = effectiveFieldNames[T](fields)
      private val f1 = fs(0)

      def convert(group: SimpleGroup, fieldName: String): Try[T] = {
        val h = implicitly[ColumnHelper[T]]
        for {
          p1 <- implicitly[ParquetCellConverter[P1]].convert(group, h.lookup(None, f1))
          t <- converter4[P2, P3, P4, P5, T](construct(p1, _, _, _, _), fs.tail.toSeq).convert(group, fieldName)
        } yield t
      }
    }

  /**
   * Method to return a ParquetCellConverter[T] where T is a 6-ary Product.
   *
   * @param construct a function (P1, P2, P3, P4, P5, P6) => T, usually the apply method of a case class.
   * @param fields    optional explicit field names; if Nil, field names are obtained by reflection.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam P6 the type of the sixth field of the Product type T.
   * @tparam T  the target case class type.
   * @return a ParquetCellConverter[T].
   */
  def converter6[P1: ParquetCellConverter, P2: ParquetCellConverter, P3: ParquetCellConverter, P4: ParquetCellConverter, P5: ParquetCellConverter, P6: ParquetCellConverter, T <: Product : ClassTag : ColumnHelper]
  (construct: (P1, P2, P3, P4, P5, P6) => T, fields: Seq[String] = Nil): ParquetCellConverter[T] =
    new ParquetCellConverter[T] {
      private val fs = effectiveFieldNames[T](fields)
      private val f1 = fs(0)

      def convert(group: SimpleGroup, fieldName: String): Try[T] = {
        val h = implicitly[ColumnHelper[T]]
        for {
          p1 <- implicitly[ParquetCellConverter[P1]].convert(group, h.lookup(None, f1))
          t <- converter5[P2, P3, P4, P5, P6, T](construct(p1, _, _, _, _, _), fs.tail.toSeq).convert(group, fieldName)
        } yield t
      }
    }

  /**
   * Method to return a ParquetCellConverter[T] where T is a 7-ary Product.
   *
   * @param construct a function (P1, P2, P3, P4, P5, P6, P7) => T, usually the apply method of a case class.
   * @param fields    optional explicit field names; if Nil, field names are obtained by reflection.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam P6 the type of the sixth field of the Product type T.
   * @tparam P7 the type of the seventh field of the Product type T.
   * @tparam T  the target case class type.
   * @return a ParquetCellConverter[T].
   */
  def converter7[P1: ParquetCellConverter, P2: ParquetCellConverter, P3: ParquetCellConverter, P4: ParquetCellConverter, P5: ParquetCellConverter, P6: ParquetCellConverter, P7: ParquetCellConverter, T <: Product : ClassTag : ColumnHelper]
  (construct: (P1, P2, P3, P4, P5, P6, P7) => T, fields: Seq[String] = Nil): ParquetCellConverter[T] =
    new ParquetCellConverter[T] {
      private val fs = effectiveFieldNames[T](fields)
      private val f1 = fs(0)

      def convert(group: SimpleGroup, fieldName: String): Try[T] = {
        val h = implicitly[ColumnHelper[T]]
        for {
          p1 <- implicitly[ParquetCellConverter[P1]].convert(group, h.lookup(None, f1))
          t <- converter6[P2, P3, P4, P5, P6, P7, T](construct(p1, _, _, _, _, _, _), fs.tail.toSeq).convert(group, fieldName)
        } yield t
      }
    }

  /**
   * Method to return a ParquetCellConverter[T] where T is an 8-ary Product.
   *
   * @param construct a function (P1, P2, P3, P4, P5, P6, P7, P8) => T, usually the apply method of a case class.
   * @param fields    optional explicit field names; if Nil, field names are obtained by reflection.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam P6 the type of the sixth field of the Product type T.
   * @tparam P7 the type of the seventh field of the Product type T.
   * @tparam P8 the type of the eighth field of the Product type T.
   * @tparam T  the target case class type.
   * @return a ParquetCellConverter[T].
   */
  def converter8[P1: ParquetCellConverter, P2: ParquetCellConverter, P3: ParquetCellConverter, P4: ParquetCellConverter, P5: ParquetCellConverter, P6: ParquetCellConverter, P7: ParquetCellConverter, P8: ParquetCellConverter, T <: Product : ClassTag : ColumnHelper]
  (construct: (P1, P2, P3, P4, P5, P6, P7, P8) => T, fields: Seq[String] = Nil): ParquetCellConverter[T] =
    new ParquetCellConverter[T] {
      private val fs = effectiveFieldNames[T](fields)
      private val f1 = fs(0)

      def convert(group: SimpleGroup, fieldName: String): Try[T] = {
        val h = implicitly[ColumnHelper[T]]
        for {
          p1 <- implicitly[ParquetCellConverter[P1]].convert(group, h.lookup(None, f1))
          t <- converter7[P2, P3, P4, P5, P6, P7, P8, T](construct(p1, _, _, _, _, _, _, _), fs.tail.toSeq).convert(group, fieldName)
        } yield t
      }
    }

  // ── Low-level dispatch methods ──────────────────────────────────────────────

  /**
   * Dispatch to the correct converter based on the field's runtime type.
   * Used by ParquetRowParser for non-optional primitive fields.
   */
  def convertField(
                          group: SimpleGroup,
                          columnName: String,
                          field: java.lang.reflect.Field
                  ): Try[Any] =
    convertByClass(group, columnName, field.getType)

  /**
   * Handle OPTIONAL columns via the legacy PrimitiveType path.
   * Used by StandardParquetRowParser for flat (non-grouped) Option fields.
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
          s"No ParquetCellConverter available for type '${other.getName}' on column '$columnName'"
        ))
    }
}