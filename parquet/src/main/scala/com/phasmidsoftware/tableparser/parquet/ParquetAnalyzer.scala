package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.table._
import java.nio.file.Path
import org.apache.parquet.schema.Type
import scala.util.Try

/**
 * Analyzer for Parquet sources (single files or datasets).
 * Reads schema and metadata from the Parquet file or dataset directory without materializing all rows.
 * Column types are inferred from the Parquet schema; statistics are lazy (None).
 *
 * @param path the path to a .parquet file or dataset directory.
 * @tparam Row the target case class type (not used for schema extraction, but kept for consistency).
 */
case class ParquetAnalyzer[Row <: Product](path: Path) extends Analyzer {
  def analyze(): ColumnStatistics = ParquetAnalysisHelper.analyzeInternal(path)
}

/**
 * Analyzer for Parquet dataset directories.
 * Same behavior as ParquetAnalyzer but validates that path is a directory.
 *
 * @param path the path to a dataset directory containing part-*.parquet files.
 * @tparam Row the target case class type.
 */
case class ParquetDatasetAnalyzer[Row <: Product](path: Path) extends Analyzer {
  def analyze(): ColumnStatistics = {
    import java.nio.file.Files
    if (!Files.isDirectory(path)) {
      throw ParquetParserException(
        s"Path is not a directory: $path. Use ParquetAnalyzer for single files.",
        None
      )
    }
    ParquetAnalysisHelper.analyzeInternal(path)
  }
}

/**
 * Helper object for shared analysis logic between single files and datasets.
 */
private object ParquetAnalysisHelper {

  /**
   * Shared internal analysis logic for both single files and datasets.
   * For directories, sums row counts from all part files.
   */
  def analyzeInternal(path: Path): ColumnStatistics = {
    import org.apache.hadoop.conf.Configuration
    import org.apache.hadoop.fs.{Path => HadoopPath}
    import org.apache.parquet.hadoop.ParquetFileReader
    import org.apache.parquet.hadoop.util.HadoopInputFile
    import scala.jdk.CollectionConverters._

    val conf = new Configuration()

    // Get schema from appropriate source (metadata or first part file)
    val schemaSourcePath = ParquetPathResolver.schemaSourcePath(path)
    val hadoopPath = new HadoopPath(schemaSourcePath.toUri)
    val reader = ParquetFileReader.open(HadoopInputFile.fromPath(hadoopPath, conf))
    try {
      val schema = reader.getFooter.getFileMetaData.getSchema
      val footer = reader.getFooter

      // For datasets without _metadata, sum all part files
      val rowCount = if (ParquetPathResolver.isDataset(path) && !ParquetPathResolver.hasMetadata(path)) {
        // Sum row counts from all part-*.parquet files
        val rowCounts = scala.collection.mutable.ArrayBuffer[Long]()
        ParquetPathResolver.allPartFiles(path).foreach { partFile =>
          val partReader = ParquetFileReader.open(HadoopInputFile.fromPath(new HadoopPath(partFile.toUri), conf))
          try {
            val count = partReader.getFooter.getBlocks.asScala.foldLeft(0L)((acc, b) => acc + b.getRowCount)
            rowCounts += count
          } finally {
            partReader.close()
          }
        }
        rowCounts.sum.toInt
      } else {
        // Single file or _metadata present: use footer blocks from schema source
        footer.getBlocks.asScala.foldLeft(0L)((acc, b) => acc + b.getRowCount).toInt
      }

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
   * Statistics are left as lazy thunks (deferred computation on demand).
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
    // Statistics deferred as lazy thunk (not computed during schema analysis)
    Column(clazz, optional, None)
  }
}

/**
 * Provider for computing column statistics from Parquet files.
 * Attempts to use Parquet metadata for eager statistics.
 * Falls back to lazy column scan if metadata unavailable and useMetadataOnly=false.
 */
object ParquetColumnStatisticsProvider extends ColumnStatisticsProvider {

  /**
   * Compute statistics for a single numeric column in a Parquet file.
   * Prefers Parquet metadata (eager, no row scan).
   * Falls back to lazy column materialization if metadata unavailable.
   *
   * @param path            the path to a .parquet file or dataset directory.
   * @param columnName      the name of the column to analyze.
   * @param useMetadataOnly if true, return None if metadata unavailable; if false, return lazy thunk.
   * @return an optional Column with eager or lazy statistics.
   */
  def computeStatistics(path: Path, columnName: String, useMetadataOnly: Boolean = true): Option[Column] = {
    import org.apache.hadoop.conf.Configuration
    import org.apache.hadoop.fs.{Path => HadoopPath}
    import org.apache.parquet.hadoop.ParquetFileReader
    import org.apache.parquet.hadoop.util.HadoopInputFile
    import org.apache.parquet.schema.PrimitiveType

    val conf = new Configuration()
    val hadoopPath = new HadoopPath(path.toUri)

    // Read schema first to validate and inspect the column
    val schemaReader = ParquetFileReader.open(HadoopInputFile.fromPath(hadoopPath, conf))
    val schema = schemaReader.getFooter.getFileMetaData.getSchema
    val footer = schemaReader.getFooter
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
        val optional = fieldType.getRepetition.toString == "OPTIONAL"
        val clazz = fieldType.asInstanceOf[PrimitiveType]
                .getPrimitiveTypeName.toString match {
          case "INT32" | "INT64" => "Int"
          case _ => "Double"
        }

        // Try to extract statistics from Parquet metadata
        val metadataStats: Option[Statistics] = extractMetadataStatistics(footer, fieldIdx)

        metadataStats match {
          case Some(stats) =>
            // Metadata available: return Column with eager statistics
            Some(Column(clazz, optional, Some(EagerStatistics(stats))))

          case None if useMetadataOnly =>
            // No metadata and user wants metadata only: return None
            None

          case None =>
            // No metadata but user allows fallback: return Column with lazy thunk
            val lazyStats: () => Option[Statistics] = () =>
              computeStatsFromRowsForColumn(path, fieldIdx)
            Some(Column(clazz, optional, Some(LazyStatistics(lazyStats))))
        }
      }
    }
  }

  /**
   * Extract statistics from Parquet column metadata if available.
   * Parquet footers may contain min, max, null_count for each column.
   *
   * @param footer   the Parquet file footer.
   * @param fieldIdx the field index.
   * @return optional Statistics if metadata is present.
   */
  private def extractMetadataStatistics(footer: org.apache.parquet.hadoop.metadata.ParquetMetadata, fieldIdx: Int): Option[Statistics] = {
    // TODO: Implement metadata extraction
    // For now, return None (fallback to lazy computation)
    None
  }

  /**
   * Compute statistics by materializing a single column from the Parquet file.
   * This is expensive; prefer metadata when available.
   * Handles both single files and dataset directories.
   *
   * @param path the path to the Parquet file or dataset directory.
   * @param fieldIdx the field index.
   * @return optional Statistics computed from row values.
   */
  private def computeStatsFromRowsForColumn(path: Path, fieldIdx: Int): Option[Statistics] = {
    import org.apache.hadoop.conf.Configuration
    import org.apache.hadoop.fs.{Path => HadoopPath}
    import org.apache.parquet.example.data.Group
    import org.apache.parquet.hadoop.ParquetReader
    import org.apache.parquet.hadoop.example.GroupReadSupport

    val conf = new Configuration()

    // For datasets, use the directory path directly with ParquetReader
    // ParquetReader.builder handles datasets automatically
    val hadoopPath = new HadoopPath(path.toUri)
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

      Statistics.make(values.toSeq)
    } finally {
      reader.close()
    }
  }
}