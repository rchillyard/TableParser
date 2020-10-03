/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.table.{Header, Indexed}
import com.phasmidsoftware.util.Reflection

import scala.reflect.ClassTag

/**
  * Trait to define various renderers for rendering instance of case classes (with their various parameters),
  * containers (Seq and Option), etc. to hierarchical output elements such as XML or HTML.
  *
  */
trait Renderers {

  /**
    * Method to return a Renderer[T].
    * You do not need to define such a renderer if you can use the default style.
    *
    * @param style the style of the resulting renderer.
    * @param attrs a set of base attributes which are explicitly set for this Renderer;
    * @tparam T the underlying type of the first parameter of the input to the render method.
    * @return a Renderer[T].
    */
  def renderer[T: Renderer](style: String, attrs: Map[String, String] = Map()): Renderer[T] = new TaggedRenderer[T](style, attrs) {}

  /**
    * Method to return a Renderer[T] where you wish to explicitly define a conversion o a T into a String.
    *
    * @param style the style of the resulting renderer.
    * @param attrs a set of base attributes which are explicitly set for this Renderer;
    * @param f     the rendering function to transform a T into a String (overrides the default asString method).
    * @tparam T the underlying type of the first parameter of the input to the render method.
    * @return a Renderer[T].
    */
  def rendererExplicit[T: Renderer](style: String, attrs: Map[String, String] = Map())(f: T => String): Renderer[T] = new TaggedRenderer[T](style, attrs) {
    override def asString(t: T): String = f(t)
  }

  /**
    * Method to return a Renderer[Header].
    *
    * CONSIDER using sequenceRenderer
    *
    * @param style the style for the header (e.g. "th" for an HTML table).
    * @param attrs the attributes.
    * @return a Renderer[ Seq[String] ]
    */
  def headerRenderer(style: String, attrs: Map[String, String] = Map(), sequenced: Boolean)(stringRenderer: Renderer[String]): Renderer[Header] = new TaggedRenderer[Header](style, attrs) {
    override def render(h: Header, attrs: Map[String, String]): Node = Node(style, attrs, headerElements(h).map((t: String) => stringRenderer.render(t)))

    private def headerElements(h: Header): Seq[String] = {
      if (sequenced) "" +: h.xs
      else h.xs
    }
  }


  /**
    * Method to return a Renderer[ Seq[T] ].
    * NOTE: there are no identifiers generated with this Renderer.
    *
    * @tparam T the underlying type of the first parameter of the input to the render method.
    * @return a Renderer[ Seq[T] ]
    */
  def sequenceRenderer[T: Renderer](style: String, attrs: Map[String, String] = Map()): Renderer[Seq[T]] = new TaggedRenderer[Seq[T]](style, attrs) {
    override def render(ts: Seq[T], attrs: Map[String, String]): Node = Node(style, attrs, ts map (implicitly[Renderer[T]].render(_)))
  }

  /**
    *
    * @param overallStyle the style of the Node which will be created by this Renderer.
    * @param indexStyle   the style of the Index node which will form part of the Node created by this Renderer.
    * @tparam T the underlying type of the first parameter of the input to the render method.
    * @return a Renderer[ Indexed[T] ].
    */
  def indexedRenderer[T: Renderer](overallStyle: String, indexStyle: String): Renderer[Indexed[T]] = new Renderer[Indexed[T]] {
    /**
      * Render an instance of Indexed[T] as a U.
      *
      * @param ti    the input parameter, i.e. the object to be rendered.
      * @param attrs a map of attributes for this value of U.
      * @return a new instance of U.
      */
    override def render(ti: Indexed[T], attrs: Map[String, String]): Node = {
      val sequence = new TaggedRenderer[Int](indexStyle) {}.render(ti.i)
      val value = implicitly[Renderer[T]].render(ti.t)
      Node(style, attrs, Seq(sequence, value))
    }

    /**
      * Defines the default style for type T.
      */
    val style: String = overallStyle
  }

  /**
    * Method to return a Renderer[ Option[T] ].
    * NOTE: there are no identifiers generated with this Renderer.
    *
    * @tparam T the underlying type of the first parameter of the input to the render method.
    * @return a Renderer[ Option[T] ].
    */
  def optionRenderer[T: Renderer](style: String, attrs: Map[String, String] = Map()): Renderer[Option[T]] = new TaggedRenderer[Option[T]](style, attrs) {
    override def render(to: Option[T], attrs: Map[String, String]): Node = to match {
      case Some(t) => Node(style, attrs, Seq(implicitly[Renderer[T]].render(t)))
      case None => Node("", None, Map(), Nil)
    }
  }

  /**
    * Method to return a Renderer[T] where T is a 1-ary Product and which is based on a function to convert a P into a T.
    *
    * NOTE: be careful using this particular method it only applies where T is a 1-tuple (e.g. a case class with one field -- not common).
    * It probably shouldn't ever be used in practice. It can cause strange initialization errors!
    * This note may be irrelevant now that we have overridden convertString to fix issue #1.
    *
    * @param construct a function P => T, usually the apply method of a case class.
    *                  The sole purpose of this function is for type inference--it is never actually invoked.
    * @tparam P1 the type of the (single) field of the Product type T.
    * @tparam T  the underlying type of the first parameter of the input to the render method.
    * @return a Renderer[T].
    */
  def renderer1[P1: Renderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: P1 => T): Renderer[T] = new ProductRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
      )
    }
  }

  /**
    * Method to return a Renderer[T] where T is a 2-ary Product and which is based on a function to convert a (P1,P2) into a T.
    *
    * @param construct a function (P1,P2) => T, usually the apply method of a case class.
    *                  The sole purpose of this function is for type inference--it is never actually invoked.
    * @tparam P1 the type of the first field of the Product type T.
    * @tparam P2 the type of the second field of the Product type T.
    * @tparam T  the underlying type of the first parameter of the input to the render method.
    * @return a Renderer[T].
    */
  def renderer2[P1: Renderer, P2: Renderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2) => T): Renderer[T] = new ProductRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
      )
    }
  }

  /**
    * Method to return a Renderer[T] where T is a 3-ary Product and which is based on a function to convert a (P1,P2,P3) into a T.
    *
    * @param construct a function (P1,P2,P3) => T, usually the apply method of a case class.
    *                  The sole purpose of this function is for type inference--it is never actually invoked.
    * @tparam P1 the type of the first field of the Product type T.
    * @tparam P2 the type of the second field of the Product type T.
    * @tparam P3 the type of the third field of the Product type T.
    * @tparam T  the underlying type of the first parameter of the input to the render method.
    * @return a Renderer[T].
    */
  def renderer3[P1: Renderer, P2: Renderer, P3: Renderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3) => T): Renderer[T] = new ProductRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
      )
    }
  }

  /**
    * Method to return a Renderer[T] where T is a 4-ary Product and which is based on a function to convert a (P1,P2,P3,P4) into a T.
    *
    * @param construct a function (P1,P2,P3,P4) => T, usually the apply method of a case class.
    *                  The sole purpose of this function is for type inference--it is never actually invoked.
    * @tparam P1 the type of the first field of the Product type T.
    * @tparam P2 the type of the second field of the Product type T.
    * @tparam P3 the type of the second field of the Product type T.
    * @tparam P4 the type of the fourth field of the Product type T.
    * @tparam T  the underlying type of the first parameter of the input to the render method.
    * @return a Renderer[T].
    */
  def renderer4[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4) => T): Renderer[T] = new ProductRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
      )
    }
  }

  /**
    * Method to return a Renderer[T] where T is a 5-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5) into a T.
    *
    * @param construct a function (P1,P2,P3,P4,P5) => T, usually the apply method of a case class.
    *                  The sole purpose of this function is for type inference--it is never actually invoked.
    * @tparam P1 the type of the first field of the Product type T.
    * @tparam P2 the type of the second field of the Product type T.
    * @tparam P3 the type of the second field of the Product type T.
    * @tparam P4 the type of the fourth field of the Product type T.
    * @tparam P5 the type of the fifth field of the Product type T.
    * @tparam T  the underlying type of the first parameter of the input to the render method.
    * @return a Renderer[T].
    */
  def renderer5[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4, P5) => T): Renderer[T] = new ProductRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4, p5) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
        , implicitly[Renderer[P5]].render(t.productElement(4).asInstanceOf[P5], nameAttr(p5))
      )
    }
  }

  /**
    * Method to return a Renderer[T] where T is a 6-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6) into a T.
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
    * @return a Renderer[T].
    */
  def renderer6[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, P6: Renderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4, P5, P6) => T): Renderer[T] = new ProductRenderer[T](style, attrs) {
    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4, p5, p6) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
        , implicitly[Renderer[P5]].render(t.productElement(4).asInstanceOf[P5], nameAttr(p5))
        , implicitly[Renderer[P6]].render(t.productElement(5).asInstanceOf[P6], nameAttr(p6))
      )
    }
  }

  /**
    * Method to return a Renderer[T] where T is a 7-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7) into a T.
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
    * @return a Renderer[T].
    */
  def renderer7[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, P6: Renderer, P7: Renderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4, P5, P6, P7) => T): Renderer[T] = new ProductRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4, p5, p6, p7) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
        , implicitly[Renderer[P5]].render(t.productElement(4).asInstanceOf[P5], nameAttr(p5))
        , implicitly[Renderer[P6]].render(t.productElement(5).asInstanceOf[P6], nameAttr(p6))
        , implicitly[Renderer[P7]].render(t.productElement(6).asInstanceOf[P7], nameAttr(p7))
      )
    }
  }

  /**
    * Method to return a Renderer[T] where T is a 8-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8) into a T.
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
    * @return a Renderer[T].
    */
  def renderer8[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, P6: Renderer, P7: Renderer, P8: Renderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4, P5, P6, P7, P8) => T): Renderer[T] = new ProductRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4, p5, p6, p7, p8) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
        , implicitly[Renderer[P5]].render(t.productElement(4).asInstanceOf[P5], nameAttr(p5))
        , implicitly[Renderer[P6]].render(t.productElement(5).asInstanceOf[P6], nameAttr(p6))
        , implicitly[Renderer[P7]].render(t.productElement(6).asInstanceOf[P7], nameAttr(p7))
        , implicitly[Renderer[P8]].render(t.productElement(7).asInstanceOf[P8], nameAttr(p8))
      )
    }
  }

  /**
    * Method to return a Renderer[T] where T is a 9-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9) into a T.
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
    * @return a Renderer[T].
    */
  def renderer9[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, P6: Renderer, P7: Renderer, P8: Renderer, P9: Renderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T): Renderer[T] = new ProductRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
        , implicitly[Renderer[P5]].render(t.productElement(4).asInstanceOf[P5], nameAttr(p5))
        , implicitly[Renderer[P6]].render(t.productElement(5).asInstanceOf[P6], nameAttr(p6))
        , implicitly[Renderer[P7]].render(t.productElement(6).asInstanceOf[P7], nameAttr(p7))
        , implicitly[Renderer[P8]].render(t.productElement(7).asInstanceOf[P8], nameAttr(p8))
        , implicitly[Renderer[P9]].render(t.productElement(8).asInstanceOf[P9], nameAttr(p9))
      )
    }
  }

  /**
    * Method to return a Renderer[T] where T is a 10-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) into a T.
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
    * @return a Renderer[T].
    */
  def renderer10[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, P6: Renderer, P7: Renderer, P8: Renderer, P9: Renderer, P10: Renderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T): Renderer[T] = new ProductRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
        , implicitly[Renderer[P5]].render(t.productElement(4).asInstanceOf[P5], nameAttr(p5))
        , implicitly[Renderer[P6]].render(t.productElement(5).asInstanceOf[P6], nameAttr(p6))
        , implicitly[Renderer[P7]].render(t.productElement(6).asInstanceOf[P7], nameAttr(p7))
        , implicitly[Renderer[P8]].render(t.productElement(7).asInstanceOf[P8], nameAttr(p8))
        , implicitly[Renderer[P9]].render(t.productElement(8).asInstanceOf[P9], nameAttr(p9))
        , implicitly[Renderer[P10]].render(t.productElement(9).asInstanceOf[P10], nameAttr(p10))
      )
    }
  }

  /**
    * Method to return a Renderer[T] where T is a 11-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11) into a T.
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
    * @return a Renderer[T].
    */
  def renderer11[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, P6: Renderer, P7: Renderer, P8: Renderer, P9: Renderer, P10: Renderer, P11: Renderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T): Renderer[T] = new ProductRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
        , implicitly[Renderer[P5]].render(t.productElement(4).asInstanceOf[P5], nameAttr(p5))
        , implicitly[Renderer[P6]].render(t.productElement(5).asInstanceOf[P6], nameAttr(p6))
        , implicitly[Renderer[P7]].render(t.productElement(6).asInstanceOf[P7], nameAttr(p7))
        , implicitly[Renderer[P8]].render(t.productElement(7).asInstanceOf[P8], nameAttr(p8))
        , implicitly[Renderer[P9]].render(t.productElement(8).asInstanceOf[P9], nameAttr(p9))
        , implicitly[Renderer[P10]].render(t.productElement(9).asInstanceOf[P10], nameAttr(p10))
        , implicitly[Renderer[P11]].render(t.productElement(10).asInstanceOf[P11], nameAttr(p11))
      )
    }
  }

  /**
    * Method to return a Renderer[T] where T is a 12-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12) into a T.
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
    * @return a Renderer[T].
    */
  def renderer12[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, P6: Renderer, P7: Renderer, P8: Renderer, P9: Renderer, P10: Renderer, P11: Renderer, P12: Renderer, T <: Product : ClassTag](style: String, attrs: Map[String, String] = Map())(construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T): Renderer[T] = new ProductRenderer[T](style, attrs) {

    protected def nodes(t: T): Seq[Node] = {
      val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) = Reflection.extractFieldNames(implicitly[ClassTag[T]])
      Seq(
        implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], nameAttr(p1))
        , implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], nameAttr(p2))
        , implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], nameAttr(p3))
        , implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], nameAttr(p4))
        , implicitly[Renderer[P5]].render(t.productElement(4).asInstanceOf[P5], nameAttr(p5))
        , implicitly[Renderer[P6]].render(t.productElement(5).asInstanceOf[P6], nameAttr(p6))
        , implicitly[Renderer[P7]].render(t.productElement(6).asInstanceOf[P7], nameAttr(p7))
        , implicitly[Renderer[P8]].render(t.productElement(7).asInstanceOf[P8], nameAttr(p8))
        , implicitly[Renderer[P9]].render(t.productElement(8).asInstanceOf[P9], nameAttr(p9))
        , implicitly[Renderer[P10]].render(t.productElement(9).asInstanceOf[P10], nameAttr(p10))
        , implicitly[Renderer[P11]].render(t.productElement(10).asInstanceOf[P11], nameAttr(p11))
        , implicitly[Renderer[P12]].render(t.productElement(11).asInstanceOf[P12], nameAttr(p12))
      )
    }
  }

  def nameAttr(value: String): Map[String, String] = Map("name" -> value)
}
