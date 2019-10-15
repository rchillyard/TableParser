/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.table.Indexed

/**
  * Trait to model behavior of something based on Rows and which is renderable.
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
    * @tparam U a class which supports TreeWriter (i.e. there is evidence of TreeWriter[U]).
    * @return a new instance of U which represents this Table as a tree of some sort.
    */
  def renderSequenced[U: TreeWriter](style: String, attributes: Map[String, String] = Map())(implicit rr: Renderer[Indexed[Row]]): U
}
