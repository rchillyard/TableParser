package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper
import org.apache.parquet.example.data.simple.SimpleGroup
import org.apache.parquet.schema.MessageType
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
 * A RowParser that reads typed values directly from a Parquet SimpleGroup,
 * bypassing the String-based CellParser machinery of core.
 *
 * @tparam Row the target case class type.
 */
trait ParquetRowParser[Row] {

  /**
   * Convert a SimpleGroup (one Parquet record) into a Try[Row].
   *
   * @param schema the Parquet MessageType, used to look up field indices.
   * @param helper the ColumnHelper providing parameter-to-column name mapping.
   * @param group  the SimpleGroup representing one row.
   * @return a Try[Row].
   */
  def parse(schema: MessageType, helper: ColumnHelper[Row])(group: SimpleGroup): Try[Row]
}

/**
 * Standard implementation of ParquetRowParser, using reflection to identify
 * the parameters of the target case class and ParquetCellConverter instances
 * to extract typed values from each field.
 *
 * @tparam Row the target case class type, must be a case class (Product).
 */
class StandardParquetRowParser[Row <: Product : ClassTag](
                                                                 converters: Seq[(String, SimpleGroup => Try[Any])]
                                                         ) extends ParquetRowParser[Row] {

  def parse(schema: MessageType, helper: ColumnHelper[Row])(group: SimpleGroup): Try[Row] =
    Try {
      val values: Seq[Any] = converters.map { case (fieldName, converter) =>
        converter(group) match {
          case Success(v) => v
          case Failure(e) => throw ParquetParserException(
            s"Failed to convert field '$fieldName'", Some(e)
          )
        }
      }
      // Use reflection to invoke the case class constructor
      val ctor = implicitly[ClassTag[Row]].runtimeClass.getConstructors.head
      ctor.newInstance(values.map(_.asInstanceOf[AnyRef]): _*).asInstanceOf[Row]
    }
}
