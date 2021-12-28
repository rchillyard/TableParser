/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.RawRow
import com.phasmidsoftware.table.{HeadedTable, Header, Table}

/**
  * Abstract class to define a raw parser, that's to say a Parser of Seq[String]
  *
  * @param maybeHeader a header if appropriate.
  * @param forgiving   true if we want this parser to be forgiving (defaults to false).
  */
abstract class RawParsers(maybeHeader: Option[Header], forgiving: Boolean = false, headerRows: Int = 1) extends CellParsers {
  self =>

  implicit val stringSeqParser: CellParser[RawRow] = cellParserSeq

  implicit val parser: StandardRowParser[RawRow] = StandardRowParser[RawRow]

  // CONSIDER why do we have a concrete Table type mentioned here?
  implicit object RawTableParser extends StringTableParser[Table[RawRow]] {
    type Row = RawRow

    val maybeFixedHeader: Option[Header] = maybeHeader

    val headerRowsToRead: Int = headerRows

    override val forgiving: Boolean = self.forgiving

    val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]

    // CONSIDER why do we have a concrete Table type mentioned here?
    protected def builder(rows: Iterable[Row], header: Header): Table[Row] = HeadedTable(rows, header)
  }

}

object RawParsers {

  object WithHeaderRow extends RawParsers(None)

}

