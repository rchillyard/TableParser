/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.table.Indexed
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
		* @tparam U the type of the result.
		* @return a new instance of U.
		*/
	def render[U: TreeWriter](t: T, attrs: Map[String, String]): U = implicitly[TreeWriter[U]].node(style, Some(asString(t)), baseAttrs ++ attrs)

	/**
		* Render an instance of T as a U.
		* This signature does not support the specification of attributes.
		*
		* @param t the input parameter, i.e. the object to be rendered.
		* @tparam U the type of the result.
		* @return a new instance of U.
		*/
	def render[U: TreeWriter](t: T): U = render(t, Map())

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
	override def render[U: TreeWriter](t: T, attrs: Map[String, String]): U = implicitly[TreeWriter[U]].node(style, attrs, us(t))

	protected def us[U: TreeWriter](t: T): Seq[U]
}

abstract class IndexedRenderer[T: Renderer](val style: String, override val baseAttrs: Map[String, String] = Map()) extends Renderer[Indexed[T]] {
	/**
		* Render an instance of Indexed[T] as a U.
		*
		* @param ti    the input parameter, i.e. the object to be rendered.
		* @param attrs a map of attributes for this value of U.
		* @tparam U the type of the result.
		* @return a new instance of U.
		*/
	override def render[U: TreeWriter](ti: Indexed[T], attrs: Map[String, String]): U = {
		// TODO specify style as header, but not with HTML-specific "th".
		val indexRenderer: Renderer[Int] = new TaggedRenderer[Int]("th") {}
		implicitly[TreeWriter[U]].node(style, Seq(indexRenderer.render(ti.i), implicitly[Renderer[T]].render(ti.t)))
	}
}

object Renderer {

	// NOTE: this is only used in unit tests
	def render[T: Renderer, U: TreeWriter](t: T): U = implicitly[Renderer[T]].render(t)

	// NOTE: this is only used in unit tests
	def render[T: Renderer, U: TreeWriter](t: T, a: String): U = implicitly[Renderer[T]].render(t, Map("name" ->a))

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

