package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.table.Table
import java.nio.file.Path
import scala.util.Try

/**
 * Entry point for parsing Parquet files and datasets into typed Tables.
 *
 * Typical usage for single files:
 * {{{
 *   given ColumnHelper[YellowTaxiTrip] =
 *     columnHelper(camelToSnakeCaseColumnNameMapper)
 *
 *   val result: Try[Table[YellowTaxiTrip]] =
 *     ParquetParser.parse[YellowTaxiTrip](Path.of("data/yellow_2024.parquet"))
 * }}}
 *
 * Typical usage for dataset directories:
 * {{{
 *   val result: Try[Table[YellowTaxiTrip]] =
 *     ParquetParser.parseDataset[YellowTaxiTrip](Path.of("data/yellow_2024"))
 * }}}
 *
 * Row's ParquetCellConverter is its own given, if it has one; otherwise it's derived
 * (see ParquetCellConverter.derived), using the ColumnHelper of each case class.
 */
object ParquetParser {

  /**
   * Parse a single Parquet file into a Table[Row].
   * Fails if path is a directory; use parseDataset for dataset directories.
   *
   * @param path a java.nio.file.Path to a .parquet file.
   * @tparam Row the target case class type.
   * @return a Try[Table[Row]].
   */
  inline def parse[Row](path: Path): Try[Table[Row]] =
    new ParquetTableParser[Row](using ParquetCellConverter[Row]).parseParquet(path)

  /**
   * Parse a Parquet dataset directory into a Table[Row].
   * Dataset directories contain multiple part-*.parquet files.
   * Fails if path is not a directory; use parse for single files.
   *
   * @param path a java.nio.file.Path to a directory containing part files.
   * @tparam Row the target case class type.
   * @return a Try[Table[Row]].
   */
  inline def parseDataset[Row](path: Path): Try[Table[Row]] =
    new ParquetTableParser[Row](using ParquetCellConverter[Row]).parseParquetDataset(path)
}