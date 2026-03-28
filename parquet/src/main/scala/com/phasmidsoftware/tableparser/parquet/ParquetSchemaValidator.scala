package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper
import com.phasmidsoftware.tableparser.core.util.Reflection
import org.apache.parquet.schema.{MessageType, PrimitiveType => ParquetPrimitiveType}
import scala.reflect.ClassTag
import scala.util.Try

/**
 * Validates a Parquet MessageType schema against a target case class,
 * prior to reading any rows.
 *
 * Supports both flat case classes (all fields map directly to Parquet columns)
 * and grouped case classes (some fields are sub-case-classes, each of which
 * maps its own fields to Parquet columns via its own ColumnHelper).
 */
object ParquetSchemaValidator {

  /**
   * Validates a Parquet MessageType schema against the target case class Row.
   *
   * Checks:
   * - Every primitive parameter of Row has a corresponding column in the Parquet schema
   * (after ColumnHelper name mapping is applied).
   * - Parameters whose type is a Product sub-class are validated recursively
   * using the provided groupedHelpers map.
   * - Every matched column has a supported Parquet type (via ParquetTypeMapper).
   * - Any OPTIONAL Parquet column must map to an Option[T] parameter in Row.
   *
   * Columns present in the Parquet schema but absent from Row are silently
   * ignored, consistent with existing CSV behaviour.
   *
   * @param schema         the MessageType read from the Parquet file footer.
   * @param helper         the ColumnHelper providing parameter-to-column name mapping for Row.
   * @param groupedHelpers a map from field name to (ClassTag, ColumnHelper) for grouped
   *                       sub-case-class fields. Defaults to empty (flat schema behaviour).
   * @tparam Row the target case class type.
   * @return Success(()) if valid, Failure(ParquetParserException) if not.
   */
  def validate[Row: ClassTag](
                                     schema: MessageType,
                                     helper: ColumnHelper[Row],
                                     groupedHelpers: Map[String, (ClassTag[_], ColumnHelper[_])] = Map.empty
                             ): Try[Unit] = Try {

    val parquetColumns: Map[String, ParquetPrimitiveType] =
      (0 until schema.getFieldCount)
              .map(schema.getType)
              .collect { case pt: ParquetPrimitiveType => pt.getName -> pt }
              .toMap

    val ct = implicitly[ClassTag[Row]]
    val fieldNames = Reflection.extractFieldNames(ct)
    val fieldMap = ct.runtimeClass.getDeclaredFields.map(f => f.getName -> f).toMap

    fieldNames.foreach { fieldName =>
      val field = fieldMap(fieldName)
      groupedHelpers.get(fieldName) match {
        case Some((subClassTag, subHelper)) =>
          validateByClassTag(subClassTag, subHelper.asInstanceOf[ColumnHelper[Any]], parquetColumns)
        case None =>
          validateSingleField(field, fieldName, helper.lookup(None, fieldName), parquetColumns)
      }
    }
  }

  /**
   * Validates the fields of a sub-case-class against the Parquet column map,
   * using Reflection.extractFieldNames to exclude synthetic fields.
   *
   * @param subClassTag    the ClassTag of the sub-case-class.
   * @param helper         the ColumnHelper for name mapping.
   * @param parquetColumns the map of available Parquet columns.
   */
  private def validateByClassTag(
                                        subClassTag: ClassTag[_],
                                        helper: ColumnHelper[Any],
                                        parquetColumns: Map[String, ParquetPrimitiveType]
                                ): Unit = {
    val fieldNames = Reflection.extractFieldNames(subClassTag)
    val fieldMap = subClassTag.runtimeClass.getDeclaredFields.map(f => f.getName -> f).toMap
    fieldNames.foreach { fieldName =>
      val field = fieldMap(fieldName)
      val columnName = helper.lookup(None, fieldName)
      validateSingleField(field, fieldName, columnName, parquetColumns)
    }
  }

  /**
   * Validates a single field against the Parquet column map.
   *
   * @param field          the reflected field.
   * @param fieldName      the Scala field name (for error messages).
   * @param columnName     the resolved Parquet column name.
   * @param parquetColumns the map of available Parquet columns.
   */
  private def validateSingleField(
                                         field: java.lang.reflect.Field,
                                         fieldName: String,
                                         columnName: String,
                                         parquetColumns: Map[String, ParquetPrimitiveType]
                                 ): Unit = {

    val parquetType = parquetColumns.getOrElse(columnName,
      throw ParquetParserException(
        s"Column '$columnName' (mapped from parameter '$fieldName') " +
                s"not found in Parquet schema. Available columns: " +
                s"${parquetColumns.keys.mkString(", ")}"
      )
    )

    ParquetTypeMapper.mapType(parquetType) match {
      case Left(ex) => throw ex
      case Right(_) => ()
    }

    val isScalaOption = field.getType == classOf[Option[_]]
    val isParquetOptional = ParquetTypeMapper.isOptional(parquetType)

    if (!isScalaOption && isParquetOptional)
      throw ParquetParserException(
        s"Column '$columnName' is OPTIONAL in Parquet schema but " +
                s"parameter '$fieldName' is not Option[_]. " +
                s"Wrap the parameter type in Option to handle null values."
      )
  }
}