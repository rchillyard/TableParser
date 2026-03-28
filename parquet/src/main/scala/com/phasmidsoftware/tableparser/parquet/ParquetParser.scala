package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper
import com.phasmidsoftware.tableparser.core.table.Table
import java.nio.file.Path
import scala.reflect.ClassTag
import scala.util.Try

/**
 * Entry point for parsing Parquet files and datasets into typed Tables.
 *
 * Typical usage for single files:
 * {{{
 *   import com.phasmidsoftware.tableparser.parquet.ParquetParser._
 *
 *   implicit val helper: ColumnHelper[YellowTaxiTrip] =
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
 */
object ParquetParser {

  /**
   * Parse a single Parquet file into a Table[Row].
   * Fails if path is a directory; use parseDataset for dataset directories.
   *
   * @param path a java.nio.file.Path to a .parquet file.
   * @param helper the ColumnHelper providing parameter-to-column name mapping.
   * @tparam Row the target case class type.
   * @return a Try[Table[Row]].
   */
  def parse[Row <: Product : ClassTag](
                                              path: Path
                                      )(implicit helper: ColumnHelper[Row]): Try[Table[Row]] = {

    val capturedHelper = helper  // capture before entering anonymous class

    val parser = new ParquetTableParser[Row] {
      val helper: ColumnHelper[Row] = capturedHelper
    }

    parser.parseParquet(path)
  }

  /**
   * Parse a Parquet dataset directory into a Table[Row].
   * Dataset directories contain multiple part-*.parquet files.
   * Fails if path is not a directory; use parse for single files.
   *
   * @param path   a java.nio.file.Path to a directory containing part files.
   * @param helper the ColumnHelper providing parameter-to-column name mapping.
   * @tparam Row the target case class type.
   * @return a Try[Table[Row]].
   */
  def parseDataset[Row <: Product : ClassTag](
                                                     path: Path
                                             )(implicit helper: ColumnHelper[Row]): Try[Table[Row]] = {

    val capturedHelper = helper  // capture before entering anonymous class

    val parser = new ParquetTableParser[Row] {
      val helper: ColumnHelper[Row] = capturedHelper
    }

    parser.parseParquetDataset(path)
  }
}