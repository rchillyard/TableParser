/*
 * Copyright (c) 2020. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.table.{Table, TableException, TableJsonFormat}
import spray.json.{JsonFormat, enrichAny}

/**
  * Abstract Class JsonRenderer which will render a Table[T] as a JsValue.
  *
  * TEST
  *
  * @tparam T the underlying type of the Table (i.e. the Row type) for which there must be evidence of JsonWriter[T].
  */
abstract class JsonRenderer[T: JsonFormat] extends StringRenderer[T] {

  def render(r: Renderable[T]): String = r match {
    case zt: Table[T] =>
      implicit object TableWriter extends TableJsonFormat[T]
      zt.toJson.prettyPrint
    case _ => throw TableException(s"render problem: unsupported type: ${r.getClass}")
  }
}


