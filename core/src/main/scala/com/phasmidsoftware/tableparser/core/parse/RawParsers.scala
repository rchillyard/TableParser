/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.table._


/**
 * Abstract class to define a raw parser, that's to say a Parser of Seq[String]
 *
 * @param maybeHeader a header if appropriate.
 * @param forgiving   true if we want this parser to be forgiving (defaults to false).
 */
abstract class RawParsers(maybeHeader: Option[Header], forgiving: Boolean = false, headerRows: Int = 1) extends CellParsers {
  self =>

  override implicit val rawRowCellParser: CellParser[RawRow] = StdCellParsers.rawRowCellParser

  implicit val parser: StandardRowParser[RawRow] = StandardRowParser.create[RawRow]

  // CONSIDER why do we have a concrete Table type mentioned here?
  implicit object RawTableParser extends StringTableParser[RawTable] {
    type Row = RawRow

    val maybeFixedHeader: Option[Header] = maybeHeader

    val headerRowsToRead: Int = headerRows

    override val forgiving: Boolean = self.forgiving

    //    val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]

    //    protected def builder(rows: Iterable[Row], header: Header): RawTable = HeadedTable(rows.map(r => RawRow(r, header)), header)

    protected def builder(rows: Iterable[Row], header: Header): RawTable = new HeadedTable(Content(rows), header)

    protected val rowParser: RowParser[Row, Input] = implicitly[RowParser[Row, String]]
  }

}

/**
 * Object `RawParsers` provides utility instances for parsing raw data represented as sequences of strings.
 * Specifically, it provides pre-configured parsers for handling different use cases.
 *
 * Example use cases include parsing data with a pre-defined header row.
 */
object RawParsers {

  /**
   * `WithHeaderRow` is an object extending `RawParsers` with a predefined configuration
   * where no specific header is provided (`None`). This parser assumes that the data contains
   * a header row, which is treated as part of the parsing logic.
   *
   * This utility is useful for scenarios where raw data includes a header row and the parser
   * should utilize it for parsing or processing the rows that follow. The header parsing itself
   * adheres to the default configurations defined in the base `RawParsers` class.
   *
   * It inherits all functionality from `RawParsers` and encapsulates the behavior of handling
   * raw data with an assumed header row.
   *
   * Example:
   * ```
   * val rawDataParser = WithHeaderRow
   * ```
   */
  object WithHeaderRow extends RawParsers(None)

}

