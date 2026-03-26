package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper
import com.phasmidsoftware.tableparser.core.table.Table
import java.nio.file.Path
import scala.reflect.ClassTag
import scala.util.Try

/**
 * Entry point for parsing Parquet files into typed Tables.
 *
 * Typical usage:
 * {{{
 *   import com.phasmidsoftware.tableparser.parquet.ParquetParser._
 *
 *   implicit val helper: ColumnHelper[YellowTaxiTrip] =
 *     columnHelper(camelToSnakeCaseColumnNameMapper)
 *
 *   val result: Try[Table[YellowTaxiTrip]] =
 *     ParquetParser.parse[YellowTaxiTrip](Path.of("data/yellow_2024"))
 * }}}
 */
object ParquetParser {

  /**
   * Parse a Parquet file or dataset directory into a Table[Row].
   *
   * @param path   a java.nio.file.Path to a .parquet file or directory.
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
}