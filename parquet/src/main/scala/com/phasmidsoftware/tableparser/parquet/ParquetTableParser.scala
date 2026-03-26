package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.parse.{ColumnHelper, RowParser, TableParser}
import com.phasmidsoftware.tableparser.core.table.{Content, HeadedTable, Header, Table}
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{Path => HadoopPath}
import org.apache.parquet.example.data.Group
import org.apache.parquet.example.data.simple.SimpleGroup
import org.apache.parquet.hadoop.ParquetReader
import org.apache.parquet.hadoop.example.GroupReadSupport
import org.apache.parquet.schema.MessageType
import java.nio.file.{Files, Path}
import scala.reflect.ClassTag
import scala.util.{Try, Using}

/**
 * A TableParser for Parquet sources.
 * Reads a Parquet file or dataset directory into a Table[Row] using
 * ParquetRowParser and ParquetSchemaValidator.
 *
 * Unlike the string-based parsers in core, this does not extend
 * AbstractTableParser -- the Parquet read path is fundamentally different.
 *
 * @tparam Row the target case class type.
 */
abstract class ParquetTableParser[Row <: Product : ClassTag]
        extends TableParser[Table[Row]] {

  type Row    // already declared by TableParser -- this is the case class type
  type Input  = SimpleGroup  // Parquet records, not Strings

  /**
   * The ColumnHelper providing parameter-to-column name mapping.
   * Users must provide this in their concrete instance, exactly as
   * they do for CSV parsing.
   */
  val helper: ColumnHelper[Row]

  /**
   * The ParquetRowParser to use for converting SimpleGroup records to Row.
   * Built lazily once the schema is known (see parse below).
   */
  protected def rowParserFor(schema: MessageType): ParquetRowParser[Row] =
    StandardParquetRowParser[Row](schema, helper)

  /**
   * Build a Table[Row] from an Iterator[Row] and a Header derived from
   * the Parquet schema.
   */
  protected def builder(rows: Iterator[Row], header: Header): Table[Row] =
    HeadedTable(Content(rows), header)

  /**
   * Not used in the Parquet path -- header comes from Parquet metadata.
   * Required by TableParser trait but never called.
   */
  val rowParser: RowParser[Row, SimpleGroup] =
    throw ParquetParserException("rowParser should not be called directly on ParquetTableParser")

  /**
   * Parse a Parquet file or dataset directory into a Table[Row].
   *
   * @param path the java.nio.file.Path to a .parquet file or directory.
   * @return a Try[Table[Row]].
   */
  def parseParquet(path: Path): Try[Table[Row]] = Try {
    val conf      = new Configuration()
    val hadoopPath = new HadoopPath(path.toUri)

    // Read the schema from the Parquet footer metadata
    val schema: MessageType = ParquetParser.readSchema(hadoopPath, conf)

    // Validate schema against the target case class
    ParquetSchemaValidator.validate[Row](schema, helper).get

    // Build the Header from Parquet column names
    val header: Header = headerFromSchema(schema)

    // Build the row parser now that we have the schema
    val rp = rowParserFor(schema)

    // Read all rows
    val rows: Iterator[Row] = readRows(hadoopPath, conf, schema, rp)

    builder(rows, header)
  }

  /**
   * The standard parse method from TableParser -- not used in the Parquet path.
   * Parquet sources should use parseParquet(path) instead.
   */
  def parse(xs: Iterator[SimpleGroup], n: Int = 0): Try[Table[Row]] =
    Failure(ParquetParserException(
      "Use parseParquet(path: java.nio.file.Path) for Parquet sources, not parse(Iterator)"
    ))

  // ── private helpers ────────────────────────────────────────────────────────

  private def headerFromSchema(schema: MessageType): Header = {
    val columnNames = (0 until schema.getFieldCount).map(schema.getFieldName)
    Header(columnNames)
  }

  private def readRows(
                              path:   HadoopPath,
                              conf:   Configuration,
                              schema: MessageType,
                              rp:     ParquetRowParser[Row]
                      ): Iterator[Row] = {

    val reader: ParquetReader[Group] =
      ParquetReader
              .builder(new GroupReadSupport(), path)
              .withConf(conf)
              .build()

    // Unfold the reader into an Iterator[Row], closing it when exhausted.
    // NOTE: ParquetReader is AutoCloseable so Using handles cleanup.
    Iterator.unfold(reader) { r =>
      Option(r.read()).map { group =>
        val row = rp.parse(schema, helper)(group.asInstanceOf[SimpleGroup])
                .fold(
                  e => throw e,
                  identity
                )
        row -> r
      }
    }
    // NOTE: the reader is not closed here -- see ParquetParser.parseParquet
    // which wraps this in a Using block at the call site.
  }
}