/*
 * Copyright (c) 2020. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.table.{Table, TableException}
import spray.json.{JsArray, JsObject, JsValue, JsonWriter, enrichAny}

/**
  * Abstract Class JsonRenderer which will render a Table[T] as a JsValue.
  *
  * @tparam T the underlying type of the Table (i.e. the Row type) for which there must be evidence of JsonWriter[T].
  */
abstract class JsonRenderer[T: JsonWriter] extends NewRenderer[T] {

  def render(r: NewRenderable[T]): String = r match {
    case zt: Table[T] => JsObject(Map[String, JsValue]("header" -> JsArray((for (row <- zt.rows) yield row.toJson).toVector))).prettyPrint
    case _ => throw TableException("render problem")
  }
}


