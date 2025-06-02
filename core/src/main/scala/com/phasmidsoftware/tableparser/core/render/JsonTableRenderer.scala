/*
 * Copyright (c) 2020. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.render

import com.phasmidsoftware.tableparser.core.table.{Table, TableJsonFormat}
import spray.json.{JsonFormat, enrichAny}

/**
 * Abstract Class JsonTableRenderer, which will render a Table[T] as a JsValue.
 *
 * @tparam T the underlying type of the Table (i.e., the Row type) for which there must be evidence of JsonWriter[T].
 */
abstract class JsonTableRenderer[T]()(implicit tj: JsonFormat[T]) extends Renderer[Table[T], String] {

  /**
   * Renders a Table[T] object into its JSON string representation with optional attributes.
   *
   * @param tt    the table to be rendered as a JSON string.
   * @param attrs a map of attributes used for customizing the rendering process (not currently used in this implementation).
   * @return a pretty-printed JSON string representation of the table.
   */
  def render(tt: Table[T], attrs: Map[String, String]): String = {
    implicit val z: JsonFormat[Table[T]] = new TableJsonFormat[T] {}
    tt.toJson.prettyPrint
  }

}


