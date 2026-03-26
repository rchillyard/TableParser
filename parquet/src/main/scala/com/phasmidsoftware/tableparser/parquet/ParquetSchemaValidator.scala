package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper
import org.apache.parquet.schema.MessageType
import org.apache.parquet.schema.PrimitiveType.PrimitiveTypeName
import org.apache.parquet.schema.Type.Repetition
import scala.reflect.ClassTag
import scala.util.Try

object ParquetSchemaValidator {

  /**
   * Validates a Parquet MessageType schema against a target case class.
   *
   * @param schema    the MessageType read from the Parquet file footer
   * @param helper    the ColumnHelper providing parameter-to-column name mapping
   * @tparam Row      the target case class type
   * @return          Unit if valid, throws ParquetParserException if not
   */
  def validate[Row: ClassTag](
                                     schema: MessageType,
                                     helper: ColumnHelper[Row]
                             ): Try[Unit] = Try {

    val parquetColumns: Map[String, ParquetPrimitiveType] =
      (0 until schema.getFieldCount)
              .map(schema.getType)
              .collect { case pt: ParquetPrimitiveType => pt.getName -> pt }
              .toMap

    val params = implicitly[ClassTag[Row]]
            .runtimeClass
            .getDeclaredFields
            .toSeq

    params.foreach { field =>
      // Map case class parameter name to expected Parquet column name
      val columnName = helper.mapColumnName(field.getName)

      // Check column exists in Parquet schema
      val parquetType = parquetColumns.getOrElse(columnName,
        throw ParquetParserException(
          s"Column '$columnName' (mapped from parameter '${field.getName}') " +
                  s"not found in Parquet schema. Available columns: " +
                  s"${parquetColumns.keys.mkString(", ")}"
        )
      )

      // Check optionality is compatible
      val isScalaOption = field.getType == classOf[Option[_]]
      val isParquetOptional = ParquetTypeMapper.isOptional(parquetType)
      if (!isScalaOption && isParquetOptional)
        throw ParquetParserException(
          s"Column '$columnName' is OPTIONAL in Parquet schema but " +
                  s"parameter '${field.getName}' is not Option[_]. " +
                  s"Wrap the parameter in Option to handle null values."
        )

      // Check type compatibility
      ParquetTypeMapper.mapType(parquetType) match {
        case Left(ex)  => throw ex
        case Right(_)  => () // type is supported; deeper type checking deferred to ParquetRowParser
      }
    }
  }
}