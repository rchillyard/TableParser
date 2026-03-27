package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper
import org.apache.parquet.schema.{MessageType, PrimitiveType => ParquetPrimitiveType}
import scala.reflect.ClassTag
import scala.util.Try

/**
 * Validates a Parquet MessageType schema against a target case class,
 * prior to reading any rows.
 */
object ParquetSchemaValidator {

  /**
   * Validates a Parquet MessageType schema against the target case class Row.
   *
   * Checks:
   * - Every parameter of Row has a corresponding column in the Parquet schema
   * (after ColumnHelper name mapping is applied)
   * - Every such column has a supported Parquet type (via ParquetTypeMapper)
   * - Any OPTIONAL Parquet column maps to an Option[T] parameter in Row
   *
   * Columns present in the Parquet schema but absent from Row are silently
   * ignored, consistent with existing CSV behaviour.
   *
   * @param schema the MessageType read from the Parquet file footer.
   * @param helper the ColumnHelper providing parameter-to-column name mapping.
   * @tparam Row the target case class type.
   * @return Success(()) if valid, Failure(ParquetParserException) if not.
   */
  def validate[Row: ClassTag](
                                     schema: MessageType,
                                     helper: ColumnHelper[Row]
                             ): Try[Unit] = Try {

    // Build a map of Parquet column name -> PrimitiveType from the schema
    val parquetColumns: Map[String, ParquetPrimitiveType] =
      (0 until schema.getFieldCount)
              .map(schema.getType)
              .collect { case pt: ParquetPrimitiveType => pt.getName -> pt }
              .toMap

    // Get the declared fields of the target case class
    val fields = implicitly[ClassTag[Row]]
            .runtimeClass
            .getDeclaredFields
            .toSeq

    fields.foreach { field =>
      // Map case class parameter name to expected Parquet column name
      val columnName = helper.lookup(None, field.getName)

      // Check column exists in Parquet schema
      val parquetType = parquetColumns.getOrElse(columnName,
        throw ParquetParserException(
          s"Column '$columnName' (mapped from parameter '${field.getName}') " +
                  s"not found in Parquet schema. Available columns: " +
                  s"${parquetColumns.keys.mkString(", ")}"
        )
      )

      // Check type is supported
      ParquetTypeMapper.mapType(parquetType) match {
        case Left(ex) => throw ex
        case Right(_) => ()
      }

      // Check optionality compatibility:
      // OPTIONAL Parquet column must map to Option[T] in the case class
      val isScalaOption = field.getType == classOf[Option[_]]
      val isParquetOptional = ParquetTypeMapper.isOptional(parquetType)

      if (!isScalaOption && isParquetOptional)
        throw ParquetParserException(
          s"Column '$columnName' is OPTIONAL in Parquet schema but " +
                  s"parameter '${field.getName}' is not Option[_]. " +
                  s"Wrap the parameter type in Option to handle null values."
        )
    }
  }
}