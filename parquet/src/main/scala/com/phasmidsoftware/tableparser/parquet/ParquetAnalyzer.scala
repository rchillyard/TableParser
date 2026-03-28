package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.util.FP
import java.nio.file.{Files, Path}
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{Path => HadoopPath}
import org.apache.parquet.example.data.Group
import org.apache.parquet.hadoop.example.GroupReadSupport
import org.apache.parquet.hadoop.metadata.ParquetMetadata
import org.apache.parquet.hadoop.util.HadoopInputFile
import org.apache.parquet.hadoop.{ParquetFileReader, ParquetReader}
import org.apache.parquet.schema.{MessageType, OriginalType, PrimitiveType, Type}
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try, Using}

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
    if (!Files.isDirectory(path))
      throw ParquetParserException(s"Path is not a directory: $path. Use ParquetAnalyzer for single files.", None)
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
   *
   * @param path the path to a .parquet file or dataset directory.
   * @return a ColumnStatistics instance describing the schema and row count.
   */
  def analyzeInternal(path: Path): ColumnStatistics = {
    val conf = new Configuration()
    val hadoopPath = new HadoopPath(ParquetPathResolver.schemaSourcePath(path).toUri)
    Using(ParquetFileReader.open(HadoopInputFile.fromPath(hadoopPath, conf))) { reader =>
      val footer = reader.getFooter
      val schema = footer.getFileMetaData.getSchema
      val rowCount =
        if (ParquetPathResolver.isDataset(path) && !ParquetPathResolver.hasMetadata(path))
          countRows(path, conf)
        else
          footer.getBlocks.asScala.foldLeft(0L)(_ + _.getRowCount).toInt
      ColumnStatistics(rowCount, schema.getFieldCount, buildColumnMap(schema))
    } match {
      case Success(value) => value
      case Failure(exception) => throw exception
    }
  }

  /**
   * Builds a map of column name to Column from a Parquet schema.
   *
   * @param schema the Parquet MessageType schema.
   * @return a Map from column name to Column.
   */
  private def buildColumnMap(schema: MessageType): Map[String, Column] =
    (0 until schema.getFieldCount).map { i =>
      val fieldType = schema.getType(i)
      fieldType.getName -> columnFromParquetType(fieldType)
    }.toMap

  /**
   * Counts the total number of rows across all part-*.parquet files in the specified path.
   *
   * @param path the root path containing Parquet part files to analyze.
   * @param conf the Hadoop Configuration used for file system access.
   * @return the total number of rows as an Int.
   */
  private def countRows(path: Path, conf: Configuration): Int =
    FP.sequenceForgiving(
      ParquetPathResolver.allPartFiles(path).map { partFile =>
        Using(ParquetFileReader.open(HadoopInputFile.fromPath(new HadoopPath(partFile.toUri), conf))) { partReader =>
          partReader.getFooter.getBlocks.asScala.foldLeft(0L)(_ + _.getRowCount)
        }
      }
    ).map(_.sum.toInt).getOrElse(throw ParquetParserException(s"Unable to count rows in $path", None))

  /**
   * Infers a Column from a Parquet field type.
   * Determines class and optionality from schema metadata; statistics are deferred (None).
   *
   * @param fieldType the Parquet Type to analyze.
   * @return a Column with clazz and optional inferred from schema, maybeStatistics = None.
   */
  private def columnFromParquetType(fieldType: Type): Column =
    Column(clazzFromParquetType(fieldType), fieldType.getRepetition.toString == "OPTIONAL", None)

  /**
   * Maps a Parquet field type to a Scala type name string.
   *
   * @param fieldType the Parquet Type to map.
   * @return a string representing the Scala type ("Int", "Double", "String", or "BigDecimal").
   */
  private def clazzFromParquetType(fieldType: Type): String = fieldType match {
    case pt: PrimitiveType =>
      pt.getPrimitiveTypeName.toString match {
        case "INT32" | "INT64" => "Int"
        case "FLOAT" | "DOUBLE" => "Double"
        case "BINARY" | "FIXED_LEN_BYTE_ARRAY" =>
          Option(pt.getOriginalType) match {
            case Some(OriginalType.DECIMAL) => "BigDecimal"
            case _ => "String"
          }
        case _ => "String"
      }
    case _ => "String"
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
    val conf = new Configuration()
    val hadoopPath = new HadoopPath(path.toUri)
    val coy = Using(ParquetFileReader.open(HadoopInputFile.fromPath(hadoopPath, conf))) { reader =>
      val footer = reader.getFooter
      val schema = footer.getFileMetaData.getSchema
      Try(schema.getFieldIndex(columnName)).toOption
              .flatMap(getFieldInformation(path, useMetadataOnly, schema, footer))
    }
    FP.recoverOption(coy)(ex => System.err.println(s"computeStatistics: Error reading Parquet file: $ex"))
  }

  /**
   * Extracts Column information for a specific field index.
   * Returns None if the field is not numeric.
   *
   * @param path            the path to the Parquet file or dataset directory.
   * @param useMetadataOnly if true, return None if metadata statistics are unavailable.
   * @param schema          the Parquet schema.
   * @param footer          the Parquet file footer containing metadata statistics.
   * @param fieldIdx        the index of the field in the schema.
   * @return an optional Column.
   */
  private def getFieldInformation(path: Path, useMetadataOnly: Boolean, schema: MessageType, footer: ParquetMetadata)(fieldIdx: Int): Option[Column] =
    numericClazz(schema, fieldIdx).flatMap { clazz =>
      val optional = schema.getType(fieldIdx).getRepetition.toString == "OPTIONAL"
      processMetadataStats(extractMetadataStatistics(footer, fieldIdx))(path, useMetadataOnly, fieldIdx, optional, clazz)
    }

  /**
   * Returns the Scala type name for a numeric Parquet field, or None if not numeric.
   *
   * @param schema   the Parquet schema.
   * @param fieldIdx the index of the field in the schema.
   * @return Some type name string if numeric, None otherwise.
   */
  private def numericClazz(schema: MessageType, fieldIdx: Int): Option[String] =
    schema.getType(fieldIdx) match {
      case pt: PrimitiveType =>
        pt.getPrimitiveTypeName.toString match {
          case "INT32" | "INT64" => Some("Int")
          case "FLOAT" | "DOUBLE" => Some("Double")
          case _ => None
        }
      case _ => None
    }

  /**
   * Processes metadata statistics and returns an appropriate Column.
   *
   * @param metadataStats   optional statistics extracted from Parquet metadata.
   * @param path            the path to a Parquet file or dataset directory.
   * @param useMetadataOnly if true, requires metadata to provide statistics.
   * @param fieldIdx        the field index of the column in the schema.
   * @param optional        true if the column is optional.
   * @param clazz           the Scala type name of the column.
   * @return an optional Column with eager or lazy statistics.
   */
  private def processMetadataStats(metadataStats: Option[Statistics])(path: Path, useMetadataOnly: Boolean, fieldIdx: Int, optional: Boolean, clazz: String): Option[Column] =
    metadataStats match {
      case Some(stats) =>
        Some(Column(clazz, optional, Some(EagerStatistics(stats))))
      case None if useMetadataOnly =>
        None
      case None =>
        Some(Column(clazz, optional, Some(LazyStatistics(() => computeStatsFromRowsForColumn(path, fieldIdx)))))
    }

  /**
   * Extract statistics from Parquet column metadata if available.
   *
   * @param footer   the Parquet file footer.
   * @param fieldIdx the field index.
   * @return optional Statistics if metadata is present.
   */
  private def extractMetadataStatistics(footer: ParquetMetadata, fieldIdx: Int): Option[Statistics] =
    // TODO: implement metadata extraction from footer block statistics
    None

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
    val reader: ParquetReader[Group] =
      ParquetReader
              .builder(new GroupReadSupport(), new HadoopPath(path.toUri))
              .withConf(new Configuration())
              .build()
    val yos = Using(reader) { r =>
      Iterator.unfold(r.read()) {
        case null => None
        case group => Some(group.getDouble(fieldIdx, 0), r.read())
      }.toSeq
    }.map(Statistics.make)
    FP.recoverOption(yos)(ex => System.err.println(s"Error reading Parquet file: $ex"))
  }
}