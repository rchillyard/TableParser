/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.{Header, Table, TableWithHeader}

/**
  * Abstract class to define a raw parser, that's to say a Parser of Seq[String]
  *
  * @param maybeHeader a header if appropriate.
  * @param forgiving   true if we want this parser to be forgiving.
  */
abstract class RawParsers(maybeHeader: Option[Header], forgiving: Boolean) extends CellParsers {
  self =>

  type StringSeq = Seq[String]

  implicit val stringSeqParser: CellParser[StringSeq] = cellParserSeq

  implicit val parser: StandardRowParser[StringSeq] = StandardRowParser[StringSeq]

  implicit object RawTableParser extends StringTableParser[Table[StringSeq]] {
    type Row = StringSeq

    val maybeFixedHeader: Option[Header] = maybeHeader

    override def forgiving: Boolean = self.forgiving

    def rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]

    def builder(rows: Seq[Row], header: Header): Table[Row] = TableWithHeader(rows, header)
  }

}

object RawParsers {

  object WithHeaderRow extends RawParsers(None, true)

}

