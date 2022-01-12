/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.{HeadedTable, Header, RawRow, RawTable}

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

    protected def builder(rows: Iterable[Row], header: Header): RawTable = HeadedTable(rows, header)

    protected val rowParser: RowParser[Row, Input] = implicitly[RowParser[Row, String]]
  }

}

object RawParsers {

  object WithHeaderRow extends RawParsers(None)

}

