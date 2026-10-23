/*
 * Copyright (c) 2026. Phasmid Software
 */

package com.phasmidsoftware.tableparser.json.table

import com.phasmidsoftware.tableparser.core.table.{Header, Table}
import io.circe.syntax.*
import io.circe.{Decoder, Encoder, Json}

/**
 * circe codecs for Table[T] (they replace 1.x's TableJsonFormat, which used spray-json).
 *
 * A Table is represented by a JSON object with a "rows" key (an array of the rows)
 * and, if the table has a header, a "header" key (an array of the column names).
 * For example:
 * {{{
 * {
 *   "rows" : [ { "playerA" : "Adam S", "playerB" : "Amy A" } ],
 *   "header" : [ "playerA", "playerB" ]
 * }
 * }}}
 *
 * Use them with `import com.phasmidsoftware.tableparser.json.table.TableCodecs.given`.
 * The rows need an Encoder (or Decoder): for a case class, `derives Codec.AsObject` gives both.
 */
object TableCodecs {

  /**
   * An Encoder for a Table[T].
   *
   * @tparam T the row type, for which there must be an Encoder[T].
   * @return an Encoder[Table[T]].
   */
  given tableEncoder[T: Encoder]: Encoder[Table[T]] = Encoder.instance { table =>
    val rows = "rows" -> Json.fromValues(table.toSeq.map(_.asJson))
    Json.fromFields(rows +: table.maybeHeader.toSeq.map(h => "header" -> h.xs.asJson))
  }

  /**
   * A Decoder for a Table[T].
   * If there is no "rows" key, the table is empty.
   * If there is no "header" key, the table has no header.
   *
   * @tparam T the row type, for which there must be a Decoder[T].
   * @return a Decoder[Table[T]].
   */
  given tableDecoder[T: Decoder]: Decoder[Table[T]] = Decoder.instance { cursor =>
    for {
      tso <- cursor.downField("rows").as[Option[List[T]]]
      wso <- cursor.downField("header").as[Option[Seq[String]]]
    } yield Table(tso.getOrElse(Nil), wso.map(ws => Header(ws, Nil)))
  }
}
