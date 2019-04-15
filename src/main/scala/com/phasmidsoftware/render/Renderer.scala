/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import org.joda.time.LocalDate

import scala.annotation.implicitNotFound
import scala.reflect.ClassTag

/**
	* Definition of type class Renderer for the purpose of serializing objects of type T.
	* Since, in general, T will be a case class which may include parameters which are case classes,
	* we render to a hierarchical type U, which therefore must have evidence of TreeWriter[U].
	*
	* @tparam T the type of object to be rendered.
	*/
@implicitNotFound(msg = "Cannot find an implicit instance of Renderer[${T}].")
trait Renderer[T] {

	/**
		* Render an instance of T as a U.
		*
		* @param t     the input parameter, i.e. the object to be rendered.
		* @param attrs a map of attributes for this value of U.
		* @return a new instance of U.
		*/
	def render(t: T, attrs: Map[String, String]): Node = Node(style, Some(asString(t)), baseAttrs ++ attrs)

	/**
		* Render an instance of T as a U.
		* This signature does not support the specification of attributes.
		*
		* @param t the input parameter, i.e. the object to be rendered.
		* @return a new instance of U.
		*/
	def render(t: T): Node = render(t, Map())

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

	/**
		* Defines the default style for type T.
		*/
	val style: String

	/**
		* Defines the base attribute set for type T.
		*/
	val baseAttrs: Map[String, String] = Map()
}

/**
	* CONSIDER having style defined as an Option[String]
	*
	* @tparam T the type of object to be rendered.
	*/
trait UntaggedRenderer[T] extends Renderer[T] {
	val style: String = ""
}

abstract class TaggedRenderer[T](val style: String, override val baseAttrs: Map[String, String] = Map()) extends Renderer[T]

abstract class ProductRenderer[T <: Product : ClassTag](val style: String, override val baseAttrs: Map[String, String] = Map()) extends Renderer[T] {
	override def render(t: T, attrs: Map[String, String]): Node = Node(style, attrs, nodes(t))

	protected def nodes(t: T): Seq[Node]
}

object Renderer {

	trait StringRenderer extends UntaggedRenderer[String]

	implicit object StringRenderer extends StringRenderer

	trait BooleanRenderer extends UntaggedRenderer[Boolean]

	implicit object BooleanRenderer extends BooleanRenderer

	trait IntRenderer extends UntaggedRenderer[Int]

	implicit object IntRenderer extends IntRenderer

	trait LongRenderer extends UntaggedRenderer[Long]

	implicit object LongRenderer extends LongRenderer

	trait BigIntRenderer extends UntaggedRenderer[BigInt]

	implicit object BigIntRenderer extends BigIntRenderer

	trait DoubleRenderer extends UntaggedRenderer[Double]

	implicit object DoubleRenderer extends DoubleRenderer

	trait LocalDateRenderer extends UntaggedRenderer[LocalDate]

	implicit object LocalDateRenderer extends LocalDateRenderer

}

