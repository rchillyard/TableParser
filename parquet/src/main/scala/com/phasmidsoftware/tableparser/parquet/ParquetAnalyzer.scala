package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.table._
import java.nio.file.Path
import org.apache.parquet.schema.Type
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Analyzer for Parquet sources.
 * Reads schema and metadata from the Parquet file without materializing all rows.
 * Column types are inferred from the Parquet schema; statistics are lazy (None).
 *
 * @param path the path to a .parquet file or dataset directory.
 * @tparam Row the target case class type (not used for schema extraction, but kept for consistency).
 */
case class ParquetAnalyzer[Row <: Product](path: Path) extends Analyzer {
  def analyze(): ColumnStatistics = {
    import org.apache.hadoop.conf.Configuration
    import org.apache.hadoop.fs.{Path => HadoopPath}
    import org.apache.parquet.hadoop.ParquetFileReader
    import org.apache.parquet.hadoop.util.HadoopInputFile

    val conf = new Configuration()
    val hadoopPath = new HadoopPath(path.toUri)
    val reader = ParquetFileReader.open(HadoopInputFile.fromPath(hadoopPath, conf))
    try {
      val schema = reader.getFooter.getFileMetaData.getSchema
      val footer = reader.getFooter
      val rowCount = footer.getBlocks.asScala.foldLeft(0L)((acc, b) => acc + b.getRowCount).toInt

      // Build columnMap from schema, inferring Column type without materializing rows
      val columnMap: Map[String, Column] = (0 until schema.getFieldCount).map { i =>
        val fieldType = schema.getType(i)
        val fieldName = fieldType.getName
        val column = columnFromParquetType(fieldType)
        fieldName -> column
      }.toMap

      ColumnStatistics(rowCount, schema.getFieldCount, columnMap)
    } finally {
      reader.close()
    }
  }

  /**
   * Infer a Column from a Parquet field type.
   * Determines class (Int, Double, String, BigDecimal) and optionality from schema metadata.
   * Statistics are left as None (lazy); they can be computed on demand via Column.statisticsFrom with a provider.
   *
   * @param fieldType the Parquet Type to analyze.
   * @return a Column with clazz and optional inferred from schema, maybeStatistics = None.
   */
  private def columnFromParquetType(fieldType: Type): Column = {
    import org.apache.parquet.schema.{OriginalType, PrimitiveType}

    val optional = fieldType.getRepetition.toString == "OPTIONAL"
    val clazz = fieldType match {
      case pt: PrimitiveType =>
        pt.getPrimitiveTypeName.toString match {
          case "INT32" | "INT64" => "Int"
          case "FLOAT" | "DOUBLE" => "Double"
          case "BINARY" | "FIXED_LEN_BYTE_ARRAY" =>
            Option(pt.getOriginalType) match {
              case Some(OriginalType.UTF8) | Some(OriginalType.JSON) => "String"
              case Some(OriginalType.DECIMAL) => "BigDecimal"
              case _ => "String"
            }
          case _ => "String"
        }
      case _ => "String"
    }
    Column(clazz, optional, None) // Statistics deferred (lazy)
  }
}

/**
 * Provider for computing column statistics from Parquet files.
 * Materializes only the target column, not the entire row.
 */
object ParquetColumnStatisticsProvider extends ColumnStatisticsProvider {

  /**
   * Compute statistics for a single numeric column in a Parquet file.
   * Reads the schema once upfront, then streams only the target column.
   *
   * @param path       the path to a .parquet file or dataset directory.
   * @param columnName the name of the column to analyze.
   * @return an optional Column with statistics computed, or None if column is non-numeric or absent.
   */
  def computeStatistics(path: Path, columnName: String): Option[Column] = {
    import org.apache.hadoop.conf.Configuration
    import org.apache.hadoop.fs.{Path => HadoopPath}
    import org.apache.parquet.example.data.Group
    import org.apache.parquet.hadoop.example.GroupReadSupport
    import org.apache.parquet.hadoop.util.HadoopInputFile
    import org.apache.parquet.hadoop.{ParquetFileReader, ParquetReader}
    import org.apache.parquet.schema.PrimitiveType

    val conf = new Configuration()
    val hadoopPath = new HadoopPath(path.toUri)

    // Read schema first to validate and inspect the column
    val schemaReader = ParquetFileReader.open(HadoopInputFile.fromPath(hadoopPath, conf))
    val schema = schemaReader.getFooter.getFileMetaData.getSchema
    schemaReader.close()

    // Check if column exists and is numeric
    Try(schema.getFieldIndex(columnName)).toOption.flatMap { fieldIdx =>
      val fieldType = schema.getType(fieldIdx)
      val isNumeric = fieldType match {
        case pt: PrimitiveType =>
          pt.getPrimitiveTypeName.toString match {
            case "INT32" | "INT64" | "FLOAT" | "DOUBLE" => true
            case _ => false
          }
        case _ => false
      }

      if (!isNumeric) None
      else {
        val reader: ParquetReader[Group] =
          ParquetReader
                  .builder(new GroupReadSupport(), hadoopPath)
                  .withConf(conf)
                  .build()

        try {
          val values = scala.collection.mutable.ArrayBuffer[Double]()
          var group = reader.read()
          while (group != null) {
            try {
              // getDouble(fieldIdx: Int, repetitionLevel: Int)
              // Use 0 for repetition level (non-repeated field)
              val value: Double = group.getDouble(fieldIdx: Int, 0)
              values += value
            } catch {
              case _: Exception => // Skip null or conversion errors
            }
            group = reader.read()
          }

          val optional = fieldType.getRepetition.toString == "OPTIONAL"
          val clazz = fieldType.asInstanceOf[PrimitiveType]
                  .getPrimitiveTypeName.toString match {
            case "INT32" | "INT64" => "Int"
            case _ => "Double"
          }

          Statistics.make(values.toSeq).map(stats => Column(clazz, optional, Some(stats)))
        } finally {
          reader.close()
        }
      }
    }
  }
}