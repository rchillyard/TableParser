/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.render

import com.phasmidsoftware.tableparser.core.table.{Header, Indexed}
import com.phasmidsoftware.tableparser.core.write
import com.phasmidsoftware.tableparser.core.write.Node

/**
 * Trait to define various renderers for rendering instance of case classes (with their various parameters),
 * containers (Seq and Option), etc. to hierarchical output elements such as XML or HTML.
 *
 */
trait HierarchicalRenderers {

  /**
   * Method to return a HierarchicalRenderer[T].
   * You do not need to define such a renderer if you can use the default style.
   *
   * @param style the style of the resulting renderer.
   * @param attrs a set of base attributes which are explicitly set for this HierarchicalRenderer;
   * @tparam T the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[T].
   */
  def renderer[T: HierarchicalRenderer](style: String, attrs: Map[String, String] = Map()): HierarchicalRenderer[T] = 
    new TaggedHierarchicalRenderer[T](style, attrs) {}

  /**
   * Method to return a HierarchicalRenderer[T] where you wish to explicitly define a conversion o a T into a String.
   *
   * TESTME
   *
   * @param style the style of the resulting renderer.
   * @param attrs a set of base attributes which are explicitly set for this HierarchicalRenderer;
   * @param f     the rendering function to transform a T into a String (overrides the default asString method).
   * @tparam T the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[T].
   */
  def rendererExplicit[T: HierarchicalRenderer](style: String, attrs: Map[String, String] = Map())(f: T => String): HierarchicalRenderer[T] = 
    new TaggedHierarchicalRenderer[T](style, attrs) {
    override def asString(t: T): String = f(t)
  }

  /**
   * Method to return a HierarchicalRenderer[Header].
   *
   * CONSIDER using sequenceRenderer
   *
   * @param style the style for the header (e.g. "th" for an HTML table).
   * @param attrs the attributes.
   * @return a HierarchicalRenderer[ Seq[String] ]
   */
  def headerRenderer(style: String, attrs: Map[String, String] = Map(), sequenced: Boolean)(renderer: HierarchicalRenderer[String]): HierarchicalRenderer[Header] = 
    new TaggedHierarchicalRenderer[Header](style, attrs) {

    /**
     * CONSIDER should we define a different sub-class of HierarchicalRenderer for this case?
     *
     * @param h     the Header.
     * @param attrs a map of attributes for this kind of output.
     * @return an instance of type String.
     */
      override def render(h: Header, attrs: Map[String, String]): Node =
        write.Node(this.style, attrs, headerElements(h).map((t: String) => renderer.render(t)))

      private def headerElements(h: Header): Seq[String] =
        if (sequenced)
          "" +: h.xs
        else
          h.xs
    }

  /**
   * Method to return a HierarchicalRenderer[ Seq[T] ].
   * NOTE: there are no identifiers generated with this HierarchicalRenderer.
   *
   * @tparam T the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[ Seq[T] ]
   */
  def sequenceRenderer[T: HierarchicalRenderer](style: String, attrs: Map[String, String] = Map()): HierarchicalRenderer[Seq[T]] = 
    new TaggedHierarchicalRenderer[Seq[T]](style, attrs) {
      override def render(ts: Seq[T], attrs: Map[String, String]): Node =
        write.Node(this.style, attrs, ts map (summon[HierarchicalRenderer[T]].render(_)))
    }

  /**
   *
   * @param overallStyle the style of the Node which will be created by this HierarchicalRenderer.
   * @param indexStyle   the style of the Index node which will form part of the Node created by this HierarchicalRenderer.
   * @tparam T the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[ Indexed[T] ].
   */
  def indexedRenderer[T: HierarchicalRenderer](overallStyle: String, indexStyle: String): HierarchicalRenderer[Indexed[T]] = 
    new HierarchicalRenderer[Indexed[T]] {
    /**
     * Render an instance of Indexed[T] as a U.
     *
     * @param ti    the input parameter, i.e. the object to be rendered.
     * @param attrs a map of attributes for this value of U.
     * @return a new instance of U.
     */
    def render(ti: Indexed[T], attrs: Map[String, String]): Node = {
      val sequence = new TaggedHierarchicalRenderer[Int](indexStyle) {}.render(ti.i)
      val value = summon[HierarchicalRenderer[T]].render(ti.t)
      write.Node(style, attrs, Seq(sequence, value))
    }

    /**
     * Defines the default style for type T.
     */
    val style: String = overallStyle
  }

  /**
   * Method to return a HierarchicalRenderer[ Option[T] ].
   * NOTE: there are no identifiers generated with this HierarchicalRenderer.
   *
   * @tparam T the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[ Option[T] ].
   */
  def optionRenderer[T: HierarchicalRenderer](style: String, attrs: Map[String, String] = Map()): HierarchicalRenderer[Option[T]] = 
    new TaggedHierarchicalRenderer[Option[T]](style, attrs) {
    override def render(to: Option[T], attrs: Map[String, String]): Node = to match {
      case Some(t) => 
        write.Node(this.style, attrs, Seq(summon[HierarchicalRenderer[T]].render(t)))
      case None => 
        Node("", None, Map(), Nil)
    }
  }
}
