/*
 * Copyright (c) 2020. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.table.{Indexed, Table}

/**
 * Polymorphic trait which defines the behavior of some sort of collection with an underlying type X and which can be rendered.
 *
 * CONSIDER: do we really need this trait?
 * This trait is not, strictly speaking, a typeclass because X does not appear as a parameter or result of any of the methods.
 * However, it cannot be sealed which implies that it is used as a typeclass.
 *
 * NOTE: this trait has no direct relationship with Renderer.
 * CONSIDER a refactoring of the whole set of traits.
 *
 * @tparam X the underlying type of this Renderable.
 */
trait Renderable[X] {

  /**
   * Method to render this Renderable to serialized type O without any constraint on O.
   *
   * CONSIDER combining this with renderToWritable.
   *
   * CONSIDER generalizing the type of ev.
   *
   * @param ev implicit evidence for Renderer of Table of X.
   * @tparam O the type of the result.
   * @return an instance of O.
   */
  def render[O](implicit ev: Renderer[Table[X], O]): O

  /**
   * CONSIDER redefining the definition of Renderer such that we can accommodate the various different types of output.
   *
   * Method to render a table in a sequential (serialized) fashion.
   *
   * @tparam O a type which supports Writable (via evidence of type Writable[O])
   * @return a new (or possibly old) instance of O.
   */
  def renderToWritable[O: Writable]: O

  /**
   * Method to render a table in a hierarchical fashion.
   *
   * TESTME
   *
   * NOTE: if your output structure is not hierarchical in nature, then simply loop through the rows of this table,
   * outputting each row as you go.
   *
   * @param style      the "style" to be used for the node which will represent this table.
   * @param attributes the attributes to be applied to the top level node for this table.
   * @param xr         an (implicit) HierarchicalRenderer[Row]
   * @tparam U a class which supports TreeWriter (i.e. there is evidence of TreeWriter[U]).
   * @return a new instance of U which represents this Table as a tree of some sort.
   */
  def renderHierarchical[U: TreeWriter](style: String, attributes: Map[String, String] = Map())(implicit xr: HierarchicalRenderer[X]): U

  /**
   * Method to render a table in a hierarchical fashion.
   *
   * TESTME
   *
   * NOTE: if your output structure is not hierarchical in nature, then simply loop through the rows of this table,
   * outputting each row as you go.
   *
   * @param style      the "style" to be used for the node which will represent this table.
   * @param attributes the attributes to be applied to the top level node for this table.
   * @param xr         an (implicit) HierarchicalRenderer[ Indexed [ Row ] ]
   * @tparam U a class which supports TreeWriter (i.e. there is evidence of TreeWriter[U]).
   * @return a new instance of U which represents this Table as a tree of some sort.
   */
  def renderHierarchicalSequenced[U: TreeWriter](style: String, attributes: Map[String, String] = Map())(implicit xr: HierarchicalRenderer[Indexed[X]]): U
}
