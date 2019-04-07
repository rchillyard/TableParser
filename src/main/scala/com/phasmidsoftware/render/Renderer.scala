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
		* @param ao an optional String which can be used to identify a particular node within a tree.
		* @tparam U the type of the result.
		* @return a new instance of U.
		*/
	def render[U: TreeWriter](t: T, ao: Option[String]): U

	/**
		* Render an instance of T as a U.
		*
		* @param t the input parameter, i.e. the object to be rendered.
		* @tparam U the type of the result.
		* @return a new instance of U.
		*/
	def render[U: TreeWriter](t: T): U = render(t, None)

}

object Renderer {

	def render[T: Renderer, U: TreeWriter](t: T): U = implicitly[Renderer[T]].render(t)

	def render[T: Renderer, U: TreeWriter](t: T, a: String): U = implicitly[Renderer[T]].render(t, Some(a))

	trait StringRenderer extends Renderer[String] {
		def render[U: TreeWriter](t: String, ao: Option[String]): U = implicitly[TreeWriter[U]].node("", Some(t), ao.toSeq, Nil)
	}

	implicit object StringRenderer extends StringRenderer

	trait BooleanRenderer extends Renderer[Boolean] {
		def render[U: TreeWriter](t: Boolean, ao: Option[String]): U = implicitly[TreeWriter[U]].node("", Some(t.toString), ao.toSeq, Nil)
	}

	implicit object BooleanRenderer extends BooleanRenderer

	trait IntRenderer extends Renderer[Int] {
		def render[U: TreeWriter](t: Int, ao: Option[String]): U = implicitly[TreeWriter[U]].node("", Some(t.toString), ao.toSeq, Nil)
	}

	implicit object IntRenderer extends IntRenderer

	trait LongRenderer extends Renderer[Long] {
		def render[U: TreeWriter](t: Long, ao: Option[String]): U = implicitly[TreeWriter[U]].node("", Some(t.toString), ao.toSeq, Nil)
	}

	implicit object LongRenderer extends LongRenderer

	trait BigIntRenderer extends Renderer[BigInt] {
		def render[U: TreeWriter](t: BigInt, ao: Option[String]): U = implicitly[TreeWriter[U]].node("", Some(t.toString), ao.toSeq, Nil)
	}

	implicit object BigIntRenderer extends BigIntRenderer

	trait DoubleRenderer extends Renderer[Double] {
		def render[U: TreeWriter](t: Double, ao: Option[String]): U = implicitly[TreeWriter[U]].node("", Some(t.toString), ao.toSeq, Nil)
	}

	implicit object DoubleRenderer extends DoubleRenderer

	trait LocalDateRenderer extends Renderer[LocalDate] {
		def render[U: TreeWriter](t: LocalDate, ao: Option[String]): U = implicitly[TreeWriter[U]].node("", Some(t.toString), ao.toSeq, Nil)
	}

	implicit object LocalDateRenderer extends LocalDateRenderer

}

