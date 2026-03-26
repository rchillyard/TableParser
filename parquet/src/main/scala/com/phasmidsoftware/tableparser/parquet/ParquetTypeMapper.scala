package com.phasmidsoftware.tableparser.parquet

import org.apache.parquet.schema.LogicalTypeAnnotation._
import org.apache.parquet.schema.PrimitiveType.PrimitiveTypeName._
import org.apache.parquet.schema.Type.Repetition
import org.apache.parquet.schema.{PrimitiveType => ParquetPrimitiveType}
import java.time.{Instant, LocalDate}

object ParquetTypeMapper {

  /**
   * Maps a Parquet PrimitiveType to a Scala Any value extracted from a SimpleGroup,
   * taking into account both the physical type and logical type annotation.
   *
   * @param pt      the Parquet PrimitiveType from the schema
   * @return a Try[Any] representing the mapped Scala value
   */
  def mapType(pt: ParquetPrimitiveType): Either[ParquetParserException, Class[_]] =
    (pt.getPrimitiveTypeName, Option(pt.getLogicalTypeAnnotation)) match {
      case (BOOLEAN, _)                                    => Right(classOf[Boolean])
      case (INT32,   Some(_: DateLogicalTypeAnnotation))   => Right(classOf[LocalDate])
      case (INT32,   Some(_: DecimalLogicalTypeAnnotation))=> Right(classOf[BigDecimal])
      case (INT32,   _)                                    => Right(classOf[Int])
      case (INT64,   Some(_: TimestampLogicalTypeAnnotation)) => Right(classOf[Instant])
      case (INT64,   Some(_: DecimalLogicalTypeAnnotation))=> Right(classOf[BigDecimal])
      case (INT64,   _)                                    => Right(classOf[Long])
      case (FLOAT,   _)                                    => Right(classOf[Float])
      case (DOUBLE,  _)                                    => Right(classOf[Double])
      case (BINARY,  _)                                    => Right(classOf[String])
      case (FIXED_LEN_BYTE_ARRAY, Some(_: DecimalLogicalTypeAnnotation)) =>
        Right(classOf[BigDecimal])
      case _ =>
        Left(ParquetParserException(
          s"Unsupported Parquet type: ${pt.getPrimitiveTypeName} " +
                  s"with logical type: ${pt.getLogicalTypeAnnotation}"
        ))
    }

  /**
   * Wraps the mapped type in Option if the column is OPTIONAL.
   */
  def isOptional(pt: ParquetPrimitiveType): Boolean =
    pt.getRepetition == Repetition.OPTIONAL
}