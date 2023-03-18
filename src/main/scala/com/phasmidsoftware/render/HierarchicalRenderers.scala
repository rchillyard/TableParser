/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.table.{Header, Indexed}
import com.phasmidsoftware.util.Reflection
import com.phasmidsoftware.write
import com.phasmidsoftware.write.Node
import scala.reflect.ClassTag

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
  def renderer[T: HierarchicalRenderer](style: String, attrs: Map[String, String] = Map()): HierarchicalRenderer[T] = new TaggedHierarchicalRenderer[T](style, attrs) {}

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
  def rendererExplicit[T: HierarchicalRenderer](style: String, attrs: Map[String, String] = Map())(f: T => String): HierarchicalRenderer[T] = new TaggedHierarchicalRenderer[T](style, attrs) {
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
  def headerRenderer(style: String, attrs: Map[String, String] = Map(), sequenced: Boolean)(renderer: HierarchicalRenderer[String]): HierarchicalRenderer[Header] = new TaggedHierarchicalRenderer[Header](style, attrs) {

    /**
     * CONSIDER should we define a different sub-class of HierarchicalRenderer for this case?
     *
     * @param h     the Header.
     * @param attrs a map of attributes for this kind of output.
     * @return an instance of type String.
     */
    override def render(h: Header, attrs: Map[String, String]): Node = write.Node(style, attrs, headerElements(h).map((t: String) => renderer.render(t)))

    private def headerElements(h: Header): Seq[String] = {
      if (sequenced) "" +: h.xs
      else h.xs
    }
  }

  /**
   * Method to return a HierarchicalRenderer[ Seq[T] ].
   * NOTE: there are no identifiers generated with this HierarchicalRenderer.
   *
   * @tparam T the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[ Seq[T] ]
   */
  def sequenceRenderer[T: HierarchicalRenderer](style: String, attrs: Map[String, String] = Map()): HierarchicalRenderer[Seq[T]] = new TaggedHierarchicalRenderer[Seq[T]](style, attrs) {
    override def render(ts: Seq[T], attrs: Map[String, String]): Node = write.Node(style, attrs, ts map (implicitly[HierarchicalRenderer[T]].render(_)))
  }

  /**
   *
   * @param overallStyle the style of the Node which will be created by this HierarchicalRenderer.
   * @param indexStyle   the style of the Index node which will form part of the Node created by this HierarchicalRenderer.
   * @tparam T the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[ Indexed[T] ].
   */
  def indexedRenderer[T: HierarchicalRenderer](overallStyle: String, indexStyle: String): HierarchicalRenderer[Indexed[T]] = new HierarchicalRenderer[Indexed[T]] {
    /**
     * Render an instance of Indexed[T] as a U.
     *
     * @param ti    the input parameter, i.e. the object to be rendered.
     * @param attrs a map of attributes for this value of U.
     * @return a new instance of U.
     */
    override def render(ti: Indexed[T], attrs: Map[String, String]): Node = {
      val sequence = new TaggedHierarchicalRenderer[Int](indexStyle) {}.render(ti.i)
      val value = implicitly[HierarchicalRenderer[T]].render(ti.t)
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
  def optionRenderer[T: HierarchicalRenderer](style: String, attrs: Map[String, String] = Map()): HierarchicalRenderer[Option[T]] = new TaggedHierarchicalRenderer[Option[T]](style, attrs) {
    override def render(to: Option[T], attrs: Map[String, String]): Node = to match {
      case Some(t) => write.Node(style, attrs, Seq(implicitly[HierarchicalRenderer[T]].render(t)))
      case None => Node("", None, Map(), Nil)
    }
  }

  /**
   * Method to return a HierarchicalRenderer[T] where T is a 1-ary Product and which is based on a function to convert a P into a T.
   *
   * NOTE: be careful using this particular method it only applies where T is a 1-tuple (e.g. a case class with one field -- not common).
   * It probably shouldn't ever be used in practice. It can cause strange initialization errors!
   * This note may be irrelevant now that we have overridden convertString to fix issue #1.
   *
   * TESTME
   *
   * @param construct a function P => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the (single) field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[T].
   */
  def renderer1[P1: HierarchicalRenderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: P1 => T): HierarchicalRenderer[T] = new ProductHierarchicalRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[HierarchicalRenderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
      )
    }
  }

  /**
   * Method to return a HierarchicalRenderer[T] where T is a 2-ary Product and which is based on a function to convert a (P1,P2) into a T.
   *
   * @param construct a function (P1,P2) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[T].
   */
  def renderer2[P1: HierarchicalRenderer, P2: HierarchicalRenderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2) => T): HierarchicalRenderer[T] = new ProductHierarchicalRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[HierarchicalRenderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[HierarchicalRenderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
      )
    }
  }

  /**
   * Method to return a HierarchicalRenderer[T] where T is a 3-ary Product and which is based on a function to convert a (P1,P2,P3) into a T.
   *
   * TESTME
   *
   * @param construct a function (P1,P2,P3) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the third field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[T].
   */
  def renderer3[P1: HierarchicalRenderer, P2: HierarchicalRenderer, P3: HierarchicalRenderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3) => T): HierarchicalRenderer[T] = new ProductHierarchicalRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[HierarchicalRenderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[HierarchicalRenderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[HierarchicalRenderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
      )
    }
  }

  /**
   * Method to return a HierarchicalRenderer[T] where T is a 4-ary Product and which is based on a function to convert a (P1,P2,P3,P4) into a T.
   *
   * TESTME
   *
   * @param construct a function (P1,P2,P3,P4) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the second field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[T].
   */
  def renderer4[P1: HierarchicalRenderer, P2: HierarchicalRenderer, P3: HierarchicalRenderer, P4: HierarchicalRenderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4) => T): HierarchicalRenderer[T] = new ProductHierarchicalRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[HierarchicalRenderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[HierarchicalRenderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[HierarchicalRenderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[HierarchicalRenderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
      )
    }
  }

  /**
   * Method to return a HierarchicalRenderer[T] where T is a 5-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5) into a T.
   *
   * @param construct a function (P1,P2,P3,P4,P5) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the second field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[T].
   */
  def renderer5[P1: HierarchicalRenderer, P2: HierarchicalRenderer, P3: HierarchicalRenderer, P4: HierarchicalRenderer, P5: HierarchicalRenderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4, P5) => T): HierarchicalRenderer[T] = new ProductHierarchicalRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4, p5) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[HierarchicalRenderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[HierarchicalRenderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[HierarchicalRenderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[HierarchicalRenderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
        , implicitly[HierarchicalRenderer[P5]].render(t.productElement(4).asInstanceOf[P5], nameAttr(p5))
      )
    }
  }

  /**
   * Method to return a HierarchicalRenderer[T] where T is a 6-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6) into a T.
   *
   * TESTME
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the second field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam P6 the type of the sixth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[T].
   */
  def renderer6[P1: HierarchicalRenderer, P2: HierarchicalRenderer, P3: HierarchicalRenderer, P4: HierarchicalRenderer, P5: HierarchicalRenderer, P6: HierarchicalRenderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4, P5, P6) => T): HierarchicalRenderer[T] = new ProductHierarchicalRenderer[T](style, attrs) {
    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4, p5, p6) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[HierarchicalRenderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[HierarchicalRenderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[HierarchicalRenderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[HierarchicalRenderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
        , implicitly[HierarchicalRenderer[P5]].render(t.productElement(4).asInstanceOf[P5], nameAttr(p5))
        , implicitly[HierarchicalRenderer[P6]].render(t.productElement(5).asInstanceOf[P6], nameAttr(p6))
      )
    }
  }

  /**
   * Method to return a HierarchicalRenderer[T] where T is a 7-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7) into a T.
   *
   * TESTME
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the second field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam P6 the type of the sixth field of the Product type T.
   * @tparam P7 the type of the seventh field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[T].
   */
  def renderer7[P1: HierarchicalRenderer, P2: HierarchicalRenderer, P3: HierarchicalRenderer, P4: HierarchicalRenderer, P5: HierarchicalRenderer, P6: HierarchicalRenderer, P7: HierarchicalRenderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4, P5, P6, P7) => T): HierarchicalRenderer[T] = new ProductHierarchicalRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4, p5, p6, p7) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[HierarchicalRenderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[HierarchicalRenderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[HierarchicalRenderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[HierarchicalRenderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
        , implicitly[HierarchicalRenderer[P5]].render(t.productElement(4).asInstanceOf[P5], nameAttr(p5))
        , implicitly[HierarchicalRenderer[P6]].render(t.productElement(5).asInstanceOf[P6], nameAttr(p6))
        , implicitly[HierarchicalRenderer[P7]].render(t.productElement(6).asInstanceOf[P7], nameAttr(p7))
      )
    }
  }

  /**
   * Method to return a HierarchicalRenderer[T] where T is a 8-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8) into a T.
   *
   * TESTME
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the second field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam P6 the type of the sixth field of the Product type T.
   * @tparam P7 the type of the seventh field of the Product type T.
   * @tparam P8 the type of the eighth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[T].
   */
  def renderer8[P1: HierarchicalRenderer, P2: HierarchicalRenderer, P3: HierarchicalRenderer, P4: HierarchicalRenderer, P5: HierarchicalRenderer, P6: HierarchicalRenderer, P7: HierarchicalRenderer, P8: HierarchicalRenderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4, P5, P6, P7, P8) => T): HierarchicalRenderer[T] = new ProductHierarchicalRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4, p5, p6, p7, p8) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[HierarchicalRenderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[HierarchicalRenderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[HierarchicalRenderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[HierarchicalRenderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
        , implicitly[HierarchicalRenderer[P5]].render(t.productElement(4).asInstanceOf[P5], nameAttr(p5))
        , implicitly[HierarchicalRenderer[P6]].render(t.productElement(5).asInstanceOf[P6], nameAttr(p6))
        , implicitly[HierarchicalRenderer[P7]].render(t.productElement(6).asInstanceOf[P7], nameAttr(p7))
        , implicitly[HierarchicalRenderer[P8]].render(t.productElement(7).asInstanceOf[P8], nameAttr(p8))
      )
    }
  }

  /**
   * Method to return a HierarchicalRenderer[T] where T is a 9-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9) into a T.
   *
   * TESTME
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1 the type of the first field of the Product type T.
   * @tparam P2 the type of the second field of the Product type T.
   * @tparam P3 the type of the second field of the Product type T.
   * @tparam P4 the type of the fourth field of the Product type T.
   * @tparam P5 the type of the fifth field of the Product type T.
   * @tparam P6 the type of the sixth field of the Product type T.
   * @tparam P7 the type of the seventh field of the Product type T.
   * @tparam P8 the type of the eighth field of the Product type T.
   * @tparam P9 the type of the ninth field of the Product type T.
   * @tparam T  the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[T].
   */
  def renderer9[P1: HierarchicalRenderer, P2: HierarchicalRenderer, P3: HierarchicalRenderer, P4: HierarchicalRenderer, P5: HierarchicalRenderer, P6: HierarchicalRenderer, P7: HierarchicalRenderer, P8: HierarchicalRenderer, P9: HierarchicalRenderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T): HierarchicalRenderer[T] = new ProductHierarchicalRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[HierarchicalRenderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[HierarchicalRenderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[HierarchicalRenderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[HierarchicalRenderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
        , implicitly[HierarchicalRenderer[P5]].render(t.productElement(4).asInstanceOf[P5], nameAttr(p5))
        , implicitly[HierarchicalRenderer[P6]].render(t.productElement(5).asInstanceOf[P6], nameAttr(p6))
        , implicitly[HierarchicalRenderer[P7]].render(t.productElement(6).asInstanceOf[P7], nameAttr(p7))
        , implicitly[HierarchicalRenderer[P8]].render(t.productElement(7).asInstanceOf[P8], nameAttr(p8))
        , implicitly[HierarchicalRenderer[P9]].render(t.productElement(8).asInstanceOf[P9], nameAttr(p9))
      )
    }
  }

  /**
   * Method to return a HierarchicalRenderer[T] where T is a 10-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) into a T.
   *
   * TESTME
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1  the type of the first field of the Product type T.
   * @tparam P2  the type of the second field of the Product type T.
   * @tparam P3  the type of the second field of the Product type T.
   * @tparam P4  the type of the fourth field of the Product type T.
   * @tparam P5  the type of the fifth field of the Product type T.
   * @tparam P6  the type of the sixth field of the Product type T.
   * @tparam P7  the type of the seventh field of the Product type T.
   * @tparam P8  the type of the eighth field of the Product type T.
   * @tparam P9  the type of the ninth field of the Product type T.
   * @tparam P10 the type of the tenth field of the Product type T.
   * @tparam T   the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[T].
   */
  def renderer10[P1: HierarchicalRenderer, P2: HierarchicalRenderer, P3: HierarchicalRenderer, P4: HierarchicalRenderer, P5: HierarchicalRenderer, P6: HierarchicalRenderer, P7: HierarchicalRenderer, P8: HierarchicalRenderer, P9: HierarchicalRenderer, P10: HierarchicalRenderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T): HierarchicalRenderer[T] = new ProductHierarchicalRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[HierarchicalRenderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[HierarchicalRenderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[HierarchicalRenderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[HierarchicalRenderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
        , implicitly[HierarchicalRenderer[P5]].render(t.productElement(4).asInstanceOf[P5], nameAttr(p5))
        , implicitly[HierarchicalRenderer[P6]].render(t.productElement(5).asInstanceOf[P6], nameAttr(p6))
        , implicitly[HierarchicalRenderer[P7]].render(t.productElement(6).asInstanceOf[P7], nameAttr(p7))
        , implicitly[HierarchicalRenderer[P8]].render(t.productElement(7).asInstanceOf[P8], nameAttr(p8))
        , implicitly[HierarchicalRenderer[P9]].render(t.productElement(8).asInstanceOf[P9], nameAttr(p9))
        , implicitly[HierarchicalRenderer[P10]].render(t.productElement(9).asInstanceOf[P10], nameAttr(p10))
      )
    }
  }

  /**
   * Method to return a HierarchicalRenderer[T] where T is a 11-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11) into a T.
   *
   * TESTME
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1  the type of the first field of the Product type T.
   * @tparam P2  the type of the second field of the Product type T.
   * @tparam P3  the type of the second field of the Product type T.
   * @tparam P4  the type of the fourth field of the Product type T.
   * @tparam P5  the type of the fifth field of the Product type T.
   * @tparam P6  the type of the sixth field of the Product type T.
   * @tparam P7  the type of the seventh field of the Product type T.
   * @tparam P8  the type of the eighth field of the Product type T.
   * @tparam P9  the type of the ninth field of the Product type T.
   * @tparam P10 the type of the tenth field of the Product type T.
   * @tparam P11 the type of the eleventh field of the Product type T.
   * @tparam T   the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[T].
   */
  def renderer11[P1: HierarchicalRenderer, P2: HierarchicalRenderer, P3: HierarchicalRenderer, P4: HierarchicalRenderer, P5: HierarchicalRenderer, P6: HierarchicalRenderer, P7: HierarchicalRenderer, P8: HierarchicalRenderer, P9: HierarchicalRenderer, P10: HierarchicalRenderer, P11: HierarchicalRenderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T): HierarchicalRenderer[T] = new ProductHierarchicalRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[HierarchicalRenderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[HierarchicalRenderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[HierarchicalRenderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[HierarchicalRenderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
        , implicitly[HierarchicalRenderer[P5]].render(t.productElement(4).asInstanceOf[P5], nameAttr(p5))
        , implicitly[HierarchicalRenderer[P6]].render(t.productElement(5).asInstanceOf[P6], nameAttr(p6))
        , implicitly[HierarchicalRenderer[P7]].render(t.productElement(6).asInstanceOf[P7], nameAttr(p7))
        , implicitly[HierarchicalRenderer[P8]].render(t.productElement(7).asInstanceOf[P8], nameAttr(p8))
        , implicitly[HierarchicalRenderer[P9]].render(t.productElement(8).asInstanceOf[P9], nameAttr(p9))
        , implicitly[HierarchicalRenderer[P10]].render(t.productElement(9).asInstanceOf[P10], nameAttr(p10))
        , implicitly[HierarchicalRenderer[P11]].render(t.productElement(10).asInstanceOf[P11], nameAttr(p11))
      )
    }
  }

  /**
   * Method to return a HierarchicalRenderer[T] where T is a 12-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12) into a T.
   *
   * TESTME
   *
   * @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12) => T, usually the apply method of a case class.
   *                  The sole purpose of this function is for type inference--it is never actually invoked.
   * @tparam P1  the type of the first field of the Product type T.
   * @tparam P2  the type of the second field of the Product type T.
   * @tparam P3  the type of the second field of the Product type T.
   * @tparam P4  the type of the fourth field of the Product type T.
   * @tparam P5  the type of the fifth field of the Product type T.
   * @tparam P6  the type of the sixth field of the Product type T.
   * @tparam P7  the type of the seventh field of the Product type T.
   * @tparam P8  the type of the eighth field of the Product type T.
   * @tparam P9  the type of the ninth field of the Product type T.
   * @tparam P10 the type of the tenth field of the Product type T.
   * @tparam P11 the type of the eleventh field of the Product type T.
   * @tparam P12 the type of the twelfth field of the Product type T.
   * @tparam T   the underlying type of the first parameter of the input to the render method.
   * @return a HierarchicalRenderer[T].
   */
  def renderer12[P1: HierarchicalRenderer, P2: HierarchicalRenderer, P3: HierarchicalRenderer, P4: HierarchicalRenderer, P5: HierarchicalRenderer, P6: HierarchicalRenderer, P7: HierarchicalRenderer, P8: HierarchicalRenderer, P9: HierarchicalRenderer, P10: HierarchicalRenderer, P11: HierarchicalRenderer, P12: HierarchicalRenderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T): HierarchicalRenderer[T] = new ProductHierarchicalRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[HierarchicalRenderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[HierarchicalRenderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[HierarchicalRenderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[HierarchicalRenderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
        , implicitly[HierarchicalRenderer[P5]].render(t.productElement(4).asInstanceOf[P5], nameAttr(p5))
        , implicitly[HierarchicalRenderer[P6]].render(t.productElement(5).asInstanceOf[P6], nameAttr(p6))
        , implicitly[HierarchicalRenderer[P7]].render(t.productElement(6).asInstanceOf[P7], nameAttr(p7))
        , implicitly[HierarchicalRenderer[P8]].render(t.productElement(7).asInstanceOf[P8], nameAttr(p8))
        , implicitly[HierarchicalRenderer[P9]].render(t.productElement(8).asInstanceOf[P9], nameAttr(p9))
        , implicitly[HierarchicalRenderer[P10]].render(t.productElement(9).asInstanceOf[P10], nameAttr(p10))
        , implicitly[HierarchicalRenderer[P11]].render(t.productElement(10).asInstanceOf[P11], nameAttr(p11))
        , implicitly[HierarchicalRenderer[P12]].render(t.productElement(11).asInstanceOf[P12], nameAttr(p12))
      )
    }
  }

  private def nameAttr(value: String): Map[String, String] = Map("name" -> value)
}
