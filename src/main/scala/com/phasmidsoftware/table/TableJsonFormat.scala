package com.phasmidsoftware.table

import spray.json.DefaultJsonProtocol._
import spray.json.{JsArray, JsObject, JsValue, JsonFormat, RootJsonFormat, enrichAny}

/**
 * Abstract class TableJsonFormat[T] which extends RootJsonFormat of Table[T]
 *
 * @tparam T which must provide evidence of JsonFormat[T].
 */
abstract class TableJsonFormat[T: JsonFormat] extends RootJsonFormat[Table[T]] {
  def write(obj: Table[T]): JsValue = {
    val jso = obj.maybeHeader map (h => h.xs.map(_.toJson))
    val jSm = Map[String, JsValue]("rows" -> JsArray((obj.rows map (_.toJson)).toVector))
    val jSm2: Map[String, JsValue] = jso match {
      case None => jSm
      case Some(js) => jSm + ("header" -> JsArray(js.toVector))
    }
    JsObject(jSm2)
  }

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
