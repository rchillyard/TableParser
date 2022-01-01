/*
 * Copyright (c) 2020. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.table.{Table, TableJsonFormat}
import spray.json.{JsonFormat, enrichAny}

/**
 * Abstract Class JsonTableRenderer which will render a Table[T] as a JsValue.
 *
 * TEST
 *
 * @tparam T the underlying type of the Table (i.e. the Row type) for which there must be evidence of JsonWriter[T].
 */
abstract class JsonTableRenderer[T]()(implicit tj: JsonFormat[T]) extends Renderer[Table[T], String] {

  def render(tt: Table[T], attrs: Map[String, String]): String = {
    implicit val z: JsonFormat[Table[T]] = new TableJsonFormat[T] {}
    tt.toJson.prettyPrint
  }

}


