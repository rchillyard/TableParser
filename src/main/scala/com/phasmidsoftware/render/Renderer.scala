/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import org.joda.time.LocalDate

import scala.annotation.implicitNotFound

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
		* @param t  the input parameter, i.e. the object to be rendered.
		* @param ao an optional String which can be used to identify a particular node within a tree,
		*           and is interpreted as an attribute.
		* @tparam U the type of the result.
		* @return a new instance of U.
		*/
	def render[U: TreeWriter](t: T, ao: Option[String]): U = implicitly[TreeWriter[U]].node(style, Some(render(t)), render(ao))

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
	def render(t: T): String = t.toString

	/**
		* Method to render the optional String (typically the field/parameter name of a case class)
		* as a sequence of Strings which will be appear as attributes in an instance of TreeWriter[U].
		*
		* If there are any T-dependent attributes that an application requires, then they should be introduced here.
		* Normally, the default method is sufficient.
		*
		* @param ao an optional String.
		* @return a sequence of Strings.
		*/
	def render(ao: Option[String]): Seq[String] = ao.toSeq

	/**
		* Defines the default style for type T.
		*/
	val style: String
}

trait UntaggedRenderer[T] extends Renderer[T] {
	val style: String = ""
}

abstract class TaggedRenderer[T](val style: String) extends Renderer[T]

object Renderer {

	// NOTE: this is only used in unit tests
	def render[T: Renderer, U: TreeWriter](t: T): U = implicitly[Renderer[T]].render(t, None)

	// NOTE: this is only used in unit tests
	def render[T: Renderer, U: TreeWriter](t: T, a: String): U = implicitly[Renderer[T]].render(t, Some(a))

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

