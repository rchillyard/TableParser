package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper
import org.apache.parquet.example.data.simple.SimpleGroup
import org.apache.parquet.schema.MessageType
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
 * Converts a Parquet SimpleGroup record directly into a typed Row,
 * bypassing the String-based CellParser machinery of core.
 *
 * @tparam Row the target case class type.
 */
trait ParquetRowParser[Row] {

  /**
   * Convert a SimpleGroup (one Parquet record) into a Try[Row].
   *
   * @param schema the Parquet MessageType, used to look up field metadata.
   * @param helper the ColumnHelper providing parameter-to-column name mapping.
   * @param group  the SimpleGroup representing one row.
   * @return a Try[Row].
   */
  def parse(schema: MessageType, helper: ColumnHelper[Row])(group: SimpleGroup): Try[Row]
}

/**
 * Standard implementation of ParquetRowParser.
 * Uses reflection to identify case class parameters and
 * ParquetCellConverter to extract typed values from each field.
 *
 * Supports both flat primitive fields and grouped sub-case-class fields.
 * A grouped field is any field whose type has an implicit ParquetCellConverter[T]
 * provided in its companion object.
 *
 * @param converters a sequence of (columnName, converter) pairs,
 *                   one per case class parameter, in declaration order.
 *                   For grouped fields, columnName is the Scala field name
 *                   (not a Parquet column name) and the converter delegates
 *                   to the sub-case-class's ParquetCellConverter.
 * @tparam Row the target case class type, must be a Product (case class).
 */
class StandardParquetRowParser[Row <: Product : ClassTag](
                                                                 converters: Seq[(String, SimpleGroup => Try[Any])]
                                                         ) extends ParquetRowParser[Row] {

  def parse(schema: MessageType, helper: ColumnHelper[Row])(group: SimpleGroup): Try[Row] =
    Try {
      val values: Seq[Any] = converters.map { case (columnName, converter) =>
        converter(group) match {
          case Success(v) => v
          case Failure(e) => throw ParquetParserException(
            s"Failed to convert field '$columnName': ${e.getClass.getSimpleName}: ${e.getMessage}",
            Some(e)
          )
        }
      }
      // Invoke the case class constructor via reflection.
      // NOTE: may fail if there are additional constructors defined for the case class.
      val ctor = implicitly[ClassTag[Row]].runtimeClass.getConstructors.head
      ctor.newInstance(values.map(_.asInstanceOf[AnyRef]): _*).asInstanceOf[Row]
    }
}

/**
 * Companion object providing a factory method to build a
 * StandardParquetRowParser from the case class structure and schema.
 */
object StandardParquetRowParser {

  /**
   * Build a StandardParquetRowParser for a given case class type.
   *
   * Reflects on the case class fields and builds a converter for each:
   * - If the field's type has an implicit ParquetCellConverter in scope,
   * that converter is used directly (supports grouped sub-case-classes).
   * - If the field type is Option[_], the legacy PrimitiveType path is used.
   * - Otherwise, the field is treated as a non-optional primitive.
   *
   * @param schema       the Parquet MessageType (already validated).
   * @param helper       the ColumnHelper providing name mapping for the top-level Row.
   * @param converterMap a map from field name to ParquetCellConverter[Any],
   *                     populated by implicit resolution for grouped sub-case-classes.
   *                     For flat schemas this map is empty.
   * @tparam Row the target case class type.
   * @return a StandardParquetRowParser[Row].
   */
  def apply[Row <: Product : ClassTag](
                                              schema: MessageType,
                                              helper: ColumnHelper[Row],
                                              converterMap: Map[String, ParquetCellConverter[Any]] = Map.empty
                                      ): StandardParquetRowParser[Row] = {

    val runtimeClass = implicitly[ClassTag[Row]].runtimeClass
    val fieldNames = com.phasmidsoftware.tableparser.core.util.Reflection
            .extractFieldNames(implicitly[ClassTag[Row]])
    val declaredFieldMap = runtimeClass.getDeclaredFields.map(f => f.getName -> f).toMap

    val converters: Seq[(String, SimpleGroup => Try[Any])] =
      fieldNames.map { fieldName =>
        val field = declaredFieldMap(fieldName)
        val columnName = helper.lookup(None, fieldName)

        val converter: SimpleGroup => Try[Any] =
          converterMap.get(fieldName) match {

            case Some(groupedConverter) =>
              // Grouped sub-case-class: delegate to its own ParquetCellConverter.
              // fieldName is used as the dispatch key; the converter ignores it
              // and reads its own columns by name via its ColumnHelper.
              group => groupedConverter.convert(group, fieldName)

            case None if field.getType == classOf[Option[_]] =>
              // Flat optional primitive: use PrimitiveType metadata path.
              val pt = schema.getType(schema.getFieldIndex(columnName)).asPrimitiveType()
              group => ParquetCellConverter.convertOption(group, columnName, pt)

            case None =>
              // Flat non-optional primitive.
              group => ParquetCellConverter.convertField(group, columnName, field)
          }

        columnName -> converter
      }

    new StandardParquetRowParser[Row](converters)
  }
}