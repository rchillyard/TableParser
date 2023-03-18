/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.write.Node
import org.joda.time.LocalDate
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

/**
 * Definition of trait Renderer for the purpose of serializing objects of type T as an object of type O.
 * This trait may be used as a type class for either T or O (or both).
 *
 * NOTE: this trait has no direct relationship with TableRenderable.
 *
 * @tparam T the (contravariant) type of object to be rendered.
 * @tparam O the type of the serialization result.
 */
@implicitNotFound(msg = "Cannot find an implicit instance of Renderer[${T},{O}].")
trait Renderer[-T, O] {

  /**
   * Render an instance of T as an O, qualifying the rendering with no attributes.
   *
   * @param t a T to be rendered.
   * @return an instance of type O.
   */
  def render(t: T): O = render(t, Map())

  /**
   * Render an instance of T as an O, qualifying the rendering with attributes defined in attrs.
   *
   * @param t     the input parameter, i.e. the T object to render.
   * @param attrs a map of attributes for this value of O.
   * @return an instance of type O.
   */
  def render(t: T, attrs: Map[String, String]): O
}

/**
 * Definition of type class HierarchicalRenderer for the purpose of serializing objects of type T.
 * Since, in general, T will be a case class which may include parameters which are case classes,
 * we render to a hierarchical type: Node.
 *
 * @tparam T the type of object to be rendered.
 */
@implicitNotFound(msg = "Cannot find an implicit instance of HierarchicalRenderer[${T}].")
trait HierarchicalRenderer[T] extends Renderer[T, Node] {

  /**
   * Defines the default style for type T.
   */
  val style: String

  /**
   * Defines the base attribute set for type T.
   */
  val baseAttrs: Map[String, String] = Map()

  /**
   * Render an instance of T as a U.
   *
   * @param t     the input parameter, i.e. the object to be rendered.
   * @param attrs a map of attributes for this value of U.
   * @return a new instance of U.
   */
  def render(t: T, attrs: Map[String, String]): Node

  /**
   * Method to render content as a String.
   * This method is invoked only when T is not a Product, sequence or Option.
   * Normally, the default method is what is required, but it might be necessary to override
   * in some situations.
   * This method does not apply to style or attribute values.
   *
   * @param t the content value.
   * @return a String corresponding to t.
   */
  def asString(t: T): String = t.toString
}

/**
 * CONSIDER having style defined as an Option[String]
 *
 * @tparam T the type of object to be rendered.
 */
trait UntaggedHierarchicalRenderer[T] extends HierarchicalRenderer[T] {
  val style: String = ""

  /**
   * Render an instance of T as a U.
   *
   * @param t     the input parameter, i.e. the object to be rendered.
   * @param attrs a map of attributes for this value of U.
   * @return a new instance of U.
   */
  def render(t: T, attrs: Map[String, String]): Node = Node(style, Some(asString(t)), baseAttrs ++ attrs)

}

/**
 * Abstract class TaggedHierarchicalRenderer.
 *
 * @param style     the style (a String)
 * @param baseAttrs baseAttrs (a Map[String, String]
 * @tparam T the type of object to be rendered.
 */
abstract class TaggedHierarchicalRenderer[T](val style: String, override val baseAttrs: Map[String, String] = Map()) extends HierarchicalRenderer[T] {

  /**
   * Render an instance of T as a U.
   *
   * @param t     the input parameter, i.e. the object to be rendered.
   * @param attrs a map of attributes for this value of U.
   * @return a new instance of U.
   */
  def render(t: T, attrs: Map[String, String]): Node = Node(style, Some(asString(t)), baseAttrs ++ attrs)
}

/**
 * Abstract class ProductHierarchicalRenderer
 *
 * @param style     the style (a String)
 * @param baseAttrs baseAttrs (a Map[String, String]
 * @tparam T the type of object to be rendered.
 */
abstract class ProductHierarchicalRenderer[T <: Product : ClassTag](val style: String, override val baseAttrs: Map[String, String] = Map()) extends HierarchicalRenderer[T] {
  def render(t: T, attrs: Map[String, String]): Node = Node(style, attrs, nodes(t))

  protected def nodes(t: T): Seq[Node]
}

object HierarchicalRenderer {

  trait StringHierarchicalRenderer extends UntaggedHierarchicalRenderer[String]

  implicit object StringHierarchicalRenderer extends StringHierarchicalRenderer

  trait BooleanHierarchicalRenderer extends UntaggedHierarchicalRenderer[Boolean]

  implicit object BooleanHierarchicalRenderer extends BooleanHierarchicalRenderer

  trait IntHierarchicalRenderer extends UntaggedHierarchicalRenderer[Int]

  implicit object IntHierarchicalRenderer extends IntHierarchicalRenderer

  trait LongHierarchicalRenderer extends UntaggedHierarchicalRenderer[Long]

  implicit object LongHierarchicalRenderer extends LongHierarchicalRenderer

  trait BigIntHierarchicalRenderer extends UntaggedHierarchicalRenderer[BigInt]

  // TESTME
  implicit object BigIntHierarchicalRenderer extends BigIntHierarchicalRenderer

  trait DoubleHierarchicalRenderer extends UntaggedHierarchicalRenderer[Double]

  implicit object DoubleHierarchicalRenderer extends DoubleHierarchicalRenderer

  trait LocalDateHierarchicalRenderer extends UntaggedHierarchicalRenderer[LocalDate]

  // TESTME
  implicit object LocalDateHierarchicalRenderer extends LocalDateHierarchicalRenderer
}
