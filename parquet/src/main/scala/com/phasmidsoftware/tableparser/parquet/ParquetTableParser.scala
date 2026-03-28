package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.parse.{ColumnHelper, TableBuilder}
import com.phasmidsoftware.tableparser.core.table.{Content, HeadedTable, Header, Table}
import java.nio.file.{Files, Path}
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{Path => HadoopPath}
import org.apache.parquet.example.data.Group
import org.apache.parquet.example.data.simple.SimpleGroup
import org.apache.parquet.hadoop.ParquetReader
import org.apache.parquet.hadoop.example.GroupReadSupport
import org.apache.parquet.schema.MessageType
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
 * A TableBuilder for Parquet sources (single files or dataset directories).
 *
 * Reads a Parquet file or dataset directory into a Table[Row] using
 * ParquetSchemaValidator, StandardParquetRowParser, and ParquetCellConverter.
 *
 * Extends TableBuilder directly rather than TableParser, since the Parquet
 * read path does not involve Iterator[String], header rows, or LineParser.
 *
 * For flat schemas (no grouped sub-case-classes), only `helper` needs to be
 * provided. For grouped schemas, override `converterMap` and `groupedHelpers`
 * in addition to `helper`.
 *
 * @tparam R the target case class type.
 */
abstract class ParquetTableParser[R <: Product : ClassTag]
        extends TableBuilder[Table[R]] {

  type Row = R

  /**
   * The ColumnHelper providing parameter-to-column name mapping for the
   * top-level Row type. Always required.
   */
  val helper: ColumnHelper[Row]

  /**
   * Map from top-level field name to ParquetCellConverter for that field's
   * sub-case-class type. Only required for grouped schemas.
   * Defaults to empty (flat schema behaviour).
   */
  val converterMap: Map[String, ParquetCellConverter[Any]] = Map.empty

  /**
   * Map from top-level field name to (ClassTag, ColumnHelper) for each
   * grouped sub-case-class field. Used by ParquetSchemaValidator to
   * recursively validate sub-class fields against the Parquet schema.
   * Defaults to empty (flat schema behaviour).
   */
  val groupedHelpers: Map[String, (ClassTag[_], ColumnHelper[_])] = Map.empty

  /**
   * Build a Table[Row] from parsed rows and a Header derived from
   * the Parquet schema. Can be overridden if a different Table
   * implementation is needed.
   */
  protected def builder(rows: Iterator[Row], header: Header): Table[Row] =
    HeadedTable(Content(rows), header)

  /**
   * Parse a single Parquet file into a Table[Row].
   * Fails if path is a directory; use parseParquetDataset for dataset directories.
   *
   * Schema validation is performed before any rows are read.
   * Fails fast with ParquetParserException on any schema mismatch
   * or unsupported type.
   *
   * @param path a java.nio.file.Path to a .parquet file (not a directory).
   * @return a Try[Table[Row]].
   */
  def parseParquet(path: Path): Try[Table[Row]] = Try {
    if (Files.isDirectory(path))
      throw ParquetParserException(
        s"Path is a directory: $path. Use parseParquetDataset for dataset directories.",
        None
      )
    parseParquetInternal(path)
  }

  /**
   * Parse a Parquet dataset directory into a Table[Row].
   * Dataset directories contain multiple part-*.parquet files.
   * Fails if path is not a directory.
   *
   * Schema validation is performed before any rows are read.
   * Fails fast with ParquetParserException on any schema mismatch
   * or unsupported type.
   *
   * @param path a java.nio.file.Path to a directory containing part files.
   * @return a Try[Table[Row]].
   */
  def parseParquetDataset(path: Path): Try[Table[Row]] = Try {
    if (!Files.isDirectory(path))
      throw ParquetParserException(
        s"Path is not a directory: $path. Use parseParquet for single files.",
        None
      )
    parseParquetInternal(path)
  }

  /**
   * Internal implementation for both single-file and dataset parsing.
   * ParquetReader.builder() handles both cases transparently.
   *
   * @param path the path to a .parquet file or dataset directory.
   * @return a Table[Row].
   */
  private def parseParquetInternal(path: Path): Table[Row] = {
    val conf = new Configuration()
    val hadoopPath = new HadoopPath(path.toUri)
    val schema: MessageType = readSchema(hadoopPath, conf)

    // Validate schema against the target case class before reading any rows.
    // For grouped schemas, groupedHelpers provides sub-class validation info.
    ParquetSchemaValidator.validate[Row](schema, helper, groupedHelpers) match {
      case Failure(exception) => throw exception
      case Success(_) =>
    }

    val header: Header = headerFromSchema(schema)

    // Build the row parser once, paying reflection cost here not per row.
    // For grouped schemas, converterMap provides sub-class converters.
    val rowParser: StandardParquetRowParser[Row] =
      StandardParquetRowParser[Row](schema, helper, converterMap)

    val rows: Iterator[Row] = readRows(hadoopPath, conf, schema, rowParser)

    builder(rows, header)
  }

  // ── private helpers ───────────────────────────────────────────────────────

  private def headerFromSchema(schema: MessageType): Header =
    Header(
      (0 until schema.getFieldCount).map(schema.getFieldName),
      Nil
    )

  private def readSchema(path: HadoopPath, conf: Configuration): MessageType = {
    import java.nio.file.Paths
    val javaPath = Paths.get(path.toUri)
    val actualPath = ParquetPathResolver.schemaSourcePath(javaPath)
    val actualHadoopPath = new HadoopPath(actualPath.toUri)
    val reader = org.apache.parquet.hadoop.ParquetFileReader.open(
      org.apache.parquet.hadoop.util.HadoopInputFile.fromPath(actualHadoopPath, conf)
    )
    try reader.getFooter.getFileMetaData.getSchema
    finally reader.close()
  }

  private def readRows(
                              path: HadoopPath,
                              conf: Configuration,
                              schema: MessageType,
                              rowParser: StandardParquetRowParser[Row]
                      ): Iterator[Row] = {
    val reader: ParquetReader[Group] =
      ParquetReader
              .builder(new GroupReadSupport(), path)
              .withConf(conf)
              .build()

    // Unfold the reader into an Iterator[Row].
    // builder() forces full materialisation into Content,
    // ensuring the reader is fully consumed before we return.
    Iterator.unfold(reader) { r =>
      Option(r.read()).map { group =>
        val row = rowParser
                .parse(schema, helper)(group.asInstanceOf[SimpleGroup])
                .fold(e => throw e, identity)
        row -> r
      }
    }
    // NOTE: reader is not explicitly closed here because builder()
    // materialises the iterator eagerly into Content via HeadedTable.
    // If you ever change builder() to return a lazy structure,
    // resource management must be revisited.
  }
}