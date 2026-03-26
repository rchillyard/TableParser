package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.parse.{ColumnHelper, TableBuilder}
import com.phasmidsoftware.tableparser.core.table.{Content, HeadedTable, Header, Table}
import java.nio.file.Path
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{Path => HadoopPath}
import org.apache.parquet.example.data.Group
import org.apache.parquet.example.data.simple.SimpleGroup
import org.apache.parquet.hadoop.ParquetReader
import org.apache.parquet.hadoop.example.GroupReadSupport
import org.apache.parquet.schema.MessageType
import scala.reflect.ClassTag
import scala.util.Try

/**
 * A TableBuilder for Parquet sources.
 *
 * Reads a Parquet file or dataset directory into a Table[Row] using
 * ParquetSchemaValidator, StandardParquetRowParser, and ParquetCellConverter.
 *
 * Extends TableBuilder directly rather than TableParser, since the Parquet
 * read path does not involve Iterator[String], header rows, or LineParser.
 *
 * @tparam R the target case class type.
 */
abstract class ParquetTableParser[R <: Product : ClassTag]
        extends TableBuilder[Table[R]] {

  type Row = R

  /**
   * The ColumnHelper providing parameter-to-column name mapping.
   * Users provide this in their concrete instance, exactly as for CSV.
   */
  val helper: ColumnHelper[Row]

  /**
   * Build a Table[Row] from parsed rows and a Header derived from
   * the Parquet schema. Can be overridden if a different Table
   * implementation is needed.
   */
  protected def builder(rows: Iterator[Row], header: Header): Table[Row] =
    HeadedTable(Content(rows), header)

  /**
   * Parse a Parquet file or dataset directory into a Table[Row].
   *
   * Schema validation is performed before any rows are read.
   * Fails fast with ParquetParserException on any schema mismatch
   * or unsupported type.
   *
   * @param path a java.nio.file.Path to a .parquet file or directory
   *             containing part files.
   * @return a Try[Table[Row]].
   */
  def parseParquet(path: Path): Try[Table[Row]] = Try {
    val conf = new Configuration()
    val hadoopPath = new HadoopPath(path.toUri)

    // Read schema from Parquet footer metadata
    val schema: MessageType = readSchema(hadoopPath, conf)

    // Validate schema against the target case class before reading any rows
    ParquetSchemaValidator.validate[Row](schema, helper).get

    // Build the Header from Parquet column names
    val header: Header = headerFromSchema(schema)

    // Build the row parser once, paying reflection cost here not per row
    val rowParser: StandardParquetRowParser[Row] =
      StandardParquetRowParser[Row](schema, helper)

    // Read and materialise all rows -- materialisation into Content
    // via builder ensures the ParquetReader is exhausted before we return,
    // avoiding any resource leak from a partially-consumed iterator.
    val rows: Iterator[Row] = readRows(hadoopPath, conf, schema, rowParser)

    builder(rows, header)
  }

  // ── private helpers ──────────────────────────────────────────────────────

  private def headerFromSchema(schema: MessageType): Header =
    Header(
      (0 until schema.getFieldCount).map(schema.getFieldName),
      Nil
    )

  private def readSchema(path: HadoopPath, conf: Configuration): MessageType = {
    val reader = org.apache.parquet.hadoop.ParquetFileReader.open(
      org.apache.parquet.hadoop.util.HadoopInputFile.fromPath(path, conf)
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
    // builder() will force full materialisation into Content,
    // ensuring the reader is fully consumed and can be closed.
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