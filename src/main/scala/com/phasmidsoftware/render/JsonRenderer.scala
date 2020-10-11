/*
 * Copyright (c) 2020. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.table.{Table, TableException}
import spray.json.{JsArray, JsObject, JsValue, JsonWriter, enrichAny}

/**
  * Abstract Class JsonRenderer which will render a Table[T] as a JsValue.
  *
  * TEST
  *
  * @tparam T the underlying type of the Table (i.e. the Row type) for which there must be evidence of JsonWriter[T].
  */
abstract class JsonRenderer[T: JsonWriter] extends StringRenderer[T] {

  import spray.json.DefaultJsonProtocol._

  def render(r: Renderable[T]): String = r match {
    case zt: Table[T] =>
      val jso = zt.maybeHeader map (h => h.xs.map(_.toJson))
      val jSm = Map[String, JsValue]("rows" -> JsArray((zt.rows map (_.toJson)).toVector))
      val jSm2: Map[String, JsValue] = jso match {
        case None => jSm
        case Some(js) => jSm + ("header" -> JsArray(js.toVector))
      }
      JsObject(jSm2).prettyPrint
    case _ => throw TableException("render problem")
  }
}


