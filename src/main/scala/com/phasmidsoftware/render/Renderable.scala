/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.table.Indexed

/**
  * Trait to model behavior of something based on Rows and which can be rendered with either a Writable or a TreeWriter.
  *
  * @tparam Row the underlying type of a row.
  */
trait Renderable[Row] {

  /**
    * Method to render a table in a sequential (serialized) fashion.
    *
    * CONSIDER does this belong here (it doesn't reference Row).
    *
    * @tparam O a type which supports Writable (via evidence of type Writable[O])
    * @return a new (or possibly old) instance of O.
    */
  def render[O: Writable]: O

  /**
    * Method to render a table in a hierarchical fashion.
    *
    * NOTE: if your output structure is not hierarchical in nature, then simply loop through the rows of this table,
    * outputting each row as you go.
    *
    * @param style      the "style" to be used for the node which will represent this table.
    * @param attributes the attributes to be applied to the top level node for this table.
    * @param rr         an (implicit) Renderer[Row]
    * @tparam U a class which supports TreeWriter (i.e. there is evidence of TreeWriter[U]).
    * @return a new instance of U which represents this Table as a tree of some sort.
    */
  def render[U: TreeWriter](style: String, attributes: Map[String, String] = Map())(implicit rr: Renderer[Row]): U

  /**
    * Method to render a table in a hierarchical fashion.
    *
    * NOTE: if your output structure is not hierarchical in nature, then simply loop through the rows of this table,
    * outputting each row as you go.
    *
    * @param style      the "style" to be used for the node which will represent this table.
    * @param attributes the attributes to be applied to the top level node for this table.
    * @param rr         an (implicit) Renderer[ Indexed [ Row ] ]
    * @return a new instance of U which represents this Table as a tree of some sort.
    */
  def renderSequenced[U: TreeWriter](style: String, attributes: Map[String, String] = Map())(implicit rr: Renderer[Indexed[Row]]): U

}

trait NewRenderable[Row] {
  /**
    * TODO introduce O parametric type for Writable.
    *
    * @return a String
    */
  def render(implicit newRenderer: NewRenderer[Row]): String = implicitly[NewRenderer[Row]].render(this)
}

/**
  * Abstract class which extends Renderable[Row] and which is designed for hierarchical output.
  *
  * @param style      the "style" to be used for the node which will represent this table.
  * @param attributes the attributes to be applied to the top level node for this table.
  * @tparam Row the underlying type of a row.
  * @tparam U   a class which supports TreeWriter (i.e. there is evidence of TreeWriter[U]).
  */
abstract class HierarchicalRenderable[Row, U: TreeWriter](style: String, attributes: Map[String, String] = Map()) extends Renderable[Row] {
  //  /**
  //    * Method to render a table in a hierarchical fashion.
  //    *
  //    * NOTE: if your output structure is not hierarchical in nature, then simply loop through the rows of this table,
  //    * outputting each row as you go.
  //    *
  //    * @param rr         an (implicit) Renderer[Row]
  //    * @return a new instance of U which represents this Table as a tree of some sort.
  //    */
  //  def render(style: String, attributes: Map[String, String] = Map())(implicit rr: Renderer[Row]): U
  //
  //  /**
  //    * Method to render a table in a hierarchical fashion.
  //    *
  //    * NOTE: if your output structure is not hierarchical in nature, then simply loop through the rows of this table,
  //    * outputting each row as you go.
  //    *
  //    * @param style      the "style" to be used for the node which will represent this table.
  //    * @param attributes the attributes to be applied to the top level node for this table.
  //    * @param rr         an (implicit) Renderer[ Indexed [ Row ] ]
  //    * @return a new instance of U which represents this Table as a tree of some sort.
  //    */
  //  def renderSequenced(style: String, attributes: Map[String, String] = Map())(implicit rr: Renderer[Indexed[Row]]): U

}
