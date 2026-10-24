package com.phasmidsoftware.tableparser.parquet

import org.apache.parquet.schema.{MessageType, PrimitiveType => ParquetPrimitiveType}
import scala.util.Try

/**
 * Validates a Parquet MessageType schema against the columns which a ParquetCellConverter reads,
 * prior to reading any rows.
 *
 * The columns come from the converter (see ParquetCellConverter.columns), so this supports both flat case classes
 * (all fields map directly to Parquet columns) and grouped case classes (some fields are sub-case-classes,
 * each of which maps its own fields to Parquet columns via its own ColumnHelper).
 */
object ParquetSchemaValidator:

  /**
   * Validates a Parquet MessageType schema against the columns read by the converter of Row.
   *
   * Checks:
   * - Every column has a corresponding column in the Parquet schema
   * (after ColumnHelper name mapping is applied).
   * - Every matched column has a supported Parquet type (via ParquetTypeMapper),
   * which maps to the type of its parameter.
   * - Any OPTIONAL Parquet column must map to an Option[T] parameter.
   *
   * Columns present in the Parquet schema but absent from Row are silently
   * ignored, consistent with existing CSV behaviour.
   *
   * @param schema    the MessageType read from the Parquet file footer.
   * @param converter the ParquetCellConverter of Row.
   * @tparam Row the target case class type.
   * @return Success(()) if valid, Failure(ParquetParserException) if not.
   */
  def validate[Row](schema: MessageType, converter: ParquetCellConverter[Row]): Try[Unit] = Try:

    val parquetColumns: Map[String, ParquetPrimitiveType] =
      (0 until schema.getFieldCount)
              .map(schema.getType)
              .collect { case pt: ParquetPrimitiveType => pt.getName -> pt }
              .toMap

    converter.columns("", "") foreach (validateColumn(_, parquetColumns))

  /**
   * Validates a single column against the Parquet column map.
   *
   * @param column         the column read by a converter.
   * @param parquetColumns the map of available Parquet columns.
   */
  private def validateColumn(column: ParquetColumn, parquetColumns: Map[String, ParquetPrimitiveType]): Unit =
    val ParquetColumn(field, columnName, maybeClass, optional) = column

    val parquetType = parquetColumns.getOrElse(columnName,
      throw ParquetParserException(
        s"Column '$columnName' (mapped from parameter '$field') " +
                s"not found in Parquet schema. Available columns: " +
                s"${parquetColumns.keys.mkString(", ")}"
      )
    )

    ParquetTypeMapper.mapType(parquetType) match
      case Left(ex) => throw ex
      case Right(clazz) =>
        maybeClass foreach { expected =>
          if (expected != clazz)
            throw ParquetParserException(
              s"Column '$columnName' (${parquetType.getPrimitiveTypeName}) maps to ${clazz.getSimpleName} " +
                      s"but parameter '$field' needs ${expected.getSimpleName}"
            )
        }

    if (!optional && ParquetTypeMapper.isOptional(parquetType))
      throw ParquetParserException(
        s"Column '$columnName' is OPTIONAL in Parquet schema but " +
                s"parameter '$field' is not Option[?]. " +
                s"Wrap the parameter type in Option to handle null values."
      )
