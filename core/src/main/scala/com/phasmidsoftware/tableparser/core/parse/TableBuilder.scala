package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.parse.TableParser.includeAll
import com.phasmidsoftware.tableparser.core.table.Header
import scala.util.Try

/**
 * Minimal trait expressing the table-building contract, independent
 * of how the input source is read.
 *
 * TableParser extends this for string-based sources.
 * ParquetTableParser extends this directly for Parquet sources.
 * Future source types (JDBC ResultSet, JSON, etc.) can do the same.
 *
 * @tparam Table the table type to be built.
 */
trait TableBuilder[Table] {

  /**
   * The row type.
   */
  type Row

  /**
   * Method to construct a Table based on the given iterator of rows and the given header.
   *
   * @param rows   an iterator of Row objects representing the data rows.
   * @param header a Header object representing the table's column headers.
   * @return the constructed Table based on the input rows and header.
   */
  protected def builder(rows: Iterator[Row], header: Header): Table

  /**
   * If true, individual row failures are logged but do not
   * cause the overall parse to fail.
   */
  protected val forgiving: Boolean = false

  /**
   * Predicate to filter rows. Defaults to including all rows.
   */
  protected val predicate: Try[Row] => Boolean = includeAll
}