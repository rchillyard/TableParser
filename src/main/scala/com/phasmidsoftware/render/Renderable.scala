/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

/**
  * Type class trait to define behavior of something which may be rendered.
  *
  * @tparam Row the Row type of an Iterable such as a Table.
  */
trait Renderable[Row] {
  /**
    * TODO introduce O parametric type for Writable.
    *
    * @return a String
    */
  def render(implicit rs: StringRenderer[Row]): String = implicitly[StringRenderer[Row]].render(this)
}
