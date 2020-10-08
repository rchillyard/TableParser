/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.RawRow
import com.phasmidsoftware.table.{HeadedArrayTable, Header, Table}

/**
  * Abstract class to define a raw parser, that's to say a Parser of Seq[String]
  *
  * @param maybeHeader a header if appropriate.
  * @param forgiving   true if we want this parser to be forgiving (defaults to false).
  */
abstract class RawParsers(maybeHeader: Option[Header], forgiving: Boolean = false) extends CellParsers {
  self =>

  implicit val stringSeqParser: CellParser[RawRow] = cellParserSeq

  implicit val parser: StandardRowParser[RawRow] = StandardRowParser[RawRow]

  implicit object RawTableParser extends StringTableParser[Table[RawRow]] {
    type Row = RawRow

    val maybeFixedHeader: Option[Header] = maybeHeader

    override val forgiving: Boolean = self.forgiving

    val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]

    protected def builder(rows: Iterator[Row], header: Header): Table[Row] = HeadedArrayTable(rows, header)
  }

}

object RawParsers {

  object WithHeaderRow extends RawParsers(None)

}

