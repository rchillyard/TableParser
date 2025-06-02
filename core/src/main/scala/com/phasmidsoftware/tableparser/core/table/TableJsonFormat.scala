package com.phasmidsoftware.tableparser.core.table

import spray.json.DefaultJsonProtocol._
import spray.json.{JsArray, JsObject, JsValue, JsonFormat, RootJsonFormat, enrichAny}

/**
 * Abstract class TableJsonFormat[T] which extends RootJsonFormat of Table[T]
 *
 * @tparam T for which there must be evidence of JsonFormat[T].
 */
abstract class TableJsonFormat[T: JsonFormat] extends RootJsonFormat[Table[T]] {
  /**
   * Serializes a `Table[T]` object into a JSON representation.
   *
   * The JSON object will contain a "rows" key representing the serialized rows of the table,
   * and, if a header exists, a "header" key that includes the serialized header.
   *
   * @param table The `Table[T]` instance to be serialized. The input must have a `maybeHeader` (optional header)
   *              and rows, where each row can be serialized to JSON.
   * @return A `JsValue` representing the serialized JSON structure of the table.
   */
  def write(table: Table[T]): JsValue = {
    val jso = table.maybeHeader map (h => h.xs.map(_.toJson))
    val jSm = Map[String, JsValue]("rows" -> JsArray((table.toSeq map (_.toJson)).toVector))
    val jSm2: Map[String, JsValue] = jso match {
      case None => jSm
      case Some(js) => jSm + ("header" -> JsArray(js.toVector))
    }
    JsObject(jSm2)
  }

  /**
   * Reads a JSON representation of a table and converts it into a `Table[T]` instance.
   *
   * The JSON object is expected to represent a table structure with optional "header" and required "rows" keys.
   * The "header" key, if present, should map to a JSON array of strings representing the table's header.
   * The "rows" key should map to a JSON array of elements convertible to the type `T`.
   *
   * @param json The JSON object to be parsed as a `Table[T]`. The structure must include:
   *             - "rows": A JSON array representing the rows of the table, where each element of the array
   *               can be converted to an instance of type `T`.
   *             - (Optional) "header": A JSON array of strings representing the header of the table.
   * @return A `Table[T]` instance parsed from the provided JSON object. If "rows" is missing, an empty table is returned.
   *         If "header" is missing, the table's header will be `None`.
   * @throws DeserializationException If the input JSON is not a valid object or cannot be parsed as a `Table[T]`.
   */
  def read(json: JsValue): Table[T] = json.asJsObject("JsonObject expected to represent Table") match {
    case JsObject(fields) =>
      def createHeader(j: JsValue): Header = Header(j.convertTo[Seq[String]], Seq[Seq[String]]())

      val ho: Option[Header] = fields.get("header") map createHeader
      val tso: Option[Iterable[T]] = fields.get("rows") map (j => j.convertTo[List[T]])
      tso match {
        case Some(ts) => Table(ts, ho)
        case None => Table(Nil, ho)
      }
  }
}
