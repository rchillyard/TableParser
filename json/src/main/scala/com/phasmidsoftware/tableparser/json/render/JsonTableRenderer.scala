/*
 * Copyright (c) 2020. Phasmid Software
 */

package com.phasmidsoftware.tableparser.json.render

import com.phasmidsoftware.tableparser.core.render.Renderer
import com.phasmidsoftware.tableparser.core.table.Table
import com.phasmidsoftware.tableparser.json.table.TableCodecs.given
import io.circe.Encoder
import io.circe.syntax.*

/**
 * Class JsonTableRenderer, which will render a Table[T] as a JSON String (see TableCodecs for the format).
 *
 * @tparam T the underlying type of the Table (i.e., the Row type) for which there must be evidence of Encoder[T].
 */
class JsonTableRenderer[T: Encoder] extends Renderer[Table[T], String] {

  /**
   * Renders a Table[T] object into its JSON string representation.
   *
   * @param tt    the table to be rendered as a JSON string.
   * @param attrs a map of attributes used for customizing the rendering process (not currently used in this implementation).
   * @return a JSON string (indented by two spaces) representing the table.
   */
  def render(tt: Table[T], attrs: Map[String, String]): String = tt.asJson.spaces2
}
