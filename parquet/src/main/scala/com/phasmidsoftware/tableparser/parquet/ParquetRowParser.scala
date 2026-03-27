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
 * @param converters a sequence of (columnName, converter) pairs,
 *                   one per case class parameter, in declaration order.
 * @tparam Row the target case class type, must be a Product (case class).
 */
class StandardParquetRowParser[Row <: Product : ClassTag](
                                                                 converters: Seq[(String, SimpleGroup => Try[Any])]
                                                         ) extends ParquetRowParser[Row] {

  def parse(schema: MessageType, helper: ColumnHelper[Row])(group: SimpleGroup): Try[Row] =
    Try {
      // Extract all field values in parameter order
      val values: Seq[Any] = converters.map { case (columnName, converter) =>
        converter(group) match {
          case Success(v) => v
          case Failure(e) => throw ParquetParserException(
            s"Failed to convert field '$columnName': ${e.getClass.getSimpleName}: ${e.getMessage}",
            Some(e)
          )
        }
      }
      // Invoke the case class constructor via reflection
      // NOTE that this may fail if there are additional constructors defined for the case class.
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
   * Reflects on the case class fields, maps each parameter name to
   * a Parquet column name via ColumnHelper, and builds a converter
   * function for each field using ParquetCellConverter.
   *
   * @param schema the Parquet MessageType (already validated).
   * @param helper the ColumnHelper providing name mapping.
   * @tparam Row the target case class type.
   * @return a StandardParquetRowParser[Row].
   */
  def apply[Row <: Product : ClassTag](
                                              schema: MessageType,
                                              helper: ColumnHelper[Row]
                                      ): StandardParquetRowParser[Row] = {

    val fields = implicitly[ClassTag[Row]]
            .runtimeClass
            .getDeclaredFields
            .toSeq

    val converters: Seq[(String, SimpleGroup => Try[Any])] =
      fields.map { field =>
        val columnName = helper.lookup(None, field.getName)
        val isOption = field.getType == classOf[Option[_]]

        val converter: SimpleGroup => Try[Any] =
          if (isOption) {
            val pt = schema.getType(schema.getFieldIndex(columnName)).asPrimitiveType()
            group => ParquetCellConverter.convertOption(group, columnName, pt)
          } else
            group => ParquetCellConverter.convertField(group, columnName, field)

        columnName -> converter
      }

    new StandardParquetRowParser[Row](converters)
  }
}