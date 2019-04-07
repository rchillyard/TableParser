/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.util.Reflection

import scala.reflect.ClassTag

/**
	* Trait to define the various renderers for writing case classes and their parameters to hierarchical output elements.
	*
	*
	*/
trait Renderers {

	/**
		* Render an sequence of P as a U.
		* NOTE: there are no identifiers generated with this Renderer.
		*
		* @tparam P the type of the result.
		* @return a new instance of U.
		*/
	def sequenceRenderer[P: Renderer](style: String): Renderer[Seq[P]] = new TaggedRenderer[Seq[P]](style) {
		override def render[U: TreeWriter](ps: Seq[P], ao: Option[String]): U = implicitly[TreeWriter[U]].node(style, None, render(ao), ps map (implicitly[Renderer[P]].render(_, None)))
	}

	/**
		* Render an option of P as a U.
		* NOTE: there are no identifiers generated with this Renderer.
		*
		* CONSIDER improving this method.
		*
		* @tparam P the type of the result.
		* @return a new instance of U.
		*/
	def optionRenderer[P: Renderer]: Renderer[Option[P]] = new UntaggedRenderer[Option[P]] {
		override def render[U: TreeWriter](po: Option[P], ao: Option[String]): U = implicitly[TreeWriter[U]].node(style, None, render(ao), po.toSeq map (implicitly[Renderer[P]].render(_, None)))
	}

	/**
		* Method to return a Renderer[T] where T is a 1-ary Product and which is based on a function to convert a P into a T.
		*
		* NOTE: be careful using this method it only applies where T is a 1-tuple (e.g. a case class with one field).
		* It probably shouldn't ever be used in practice. It can cause strange initialization errors!
		* This note may be irrelevant now that we have overridden convertString to fix issue #1.
		*
		* @param style the style of the resulting renderer.
		* @tparam T the type of the element to be rendered.
		* @return a Renderer which converts an instance of T into an instance of U.
		*/
	def renderer[T: Renderer](style: String): Renderer[T] = new TaggedRenderer[T](style) {}

	/**
		* Method to return a Renderer[T] where T is a 1-ary Product and which is based on a function to convert a P into a T.
		*
		* NOTE: be careful using this method it only applies where T is a 1-tuple (e.g. a case class with one field).
		* It probably shouldn't ever be used in practice. It can cause strange initialization errors!
		* This note may be irrelevant now that we have overridden convertString to fix issue #1.
		*
		* @param style the String which will be used for the "tag" parameter of U's node method.
		* @param f     the rendering function to transform a T into a String (overrides the default render method).
		* @param g     the rendering function to transform a Option[String] into a Seq[String] (overrides the default render method).
		* @tparam T the type of the element to be rendered.
		* @return a Renderer which converts an instance of T into an instance of U.
		*/
	def renderer[T: Renderer](style: String)(f: T => String, g: Option[String] => Seq[String]): Renderer[T] = new TaggedRenderer[T](style) {

		override def render(t: T): String = f(t)

		override def render(ao: Option[String]): Seq[String] = g(ao)
	}

	/**
		* Method to return a Renderer[T] where T is a 1-ary Product and which is based on a function to convert a P into a T.
		*
		* NOTE: be careful using this method it only applies where T is a 1-tuple (e.g. a case class with one field).
		* It probably shouldn't ever be used in practice. It can cause strange initialization errors!
		* This note may be irrelevant now that we have overridden convertString to fix issue #1.
		*
		* @param construct a function P => T, usually the apply method of a case class.
		* @tparam P1 the type of the (single) field of the Product type T.
		* @tparam T  the underlying type of the result, a Product.
		* @return a Renderer which converts a String from a Row into the field type P and thence into a T
		*/
	def renderer1[P1: Renderer, T <: Product : ClassTag](style: String)(construct: P1 => T): Renderer[T] = new TaggedRenderer[T](style) {
		val Array(p1) = Reflection.extractFieldNames(implicitly[ClassTag[T]])

		override def render[U: TreeWriter](t: T, ao: Option[String]): U = implicitly[TreeWriter[U]].node(style, None, render(ao), Seq(
			implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], Some(p1))
		))
	}

	/**
		* Method to return a Renderer[T] where T is a 2-ary Product and which is based on a function to convert a (P1,P2) into a T.
		*
		* @param construct a function (P1,P2) => T, usually the apply method of a case class.
		* @tparam P1 the type of the first field of the Product type T.
		* @tparam P2 the type of the second field of the Product type T.
		* @tparam T  the underlying type of the result, a Product.
		* @return a Renderer which converts Strings from a Row into the field types P1 and P2 and thence into a T
		*/
	def renderer2[P1: Renderer, P2: Renderer, T <: Product : ClassTag](style: String)(construct: (P1, P2) => T): Renderer[T] = new TaggedRenderer[T](style) {
		val Array(p1, p2) = Reflection.extractFieldNames(implicitly[ClassTag[T]])

		override def render[U: TreeWriter](t: T, ao: Option[String]): U = implicitly[TreeWriter[U]].node(style, None, render(ao), Seq(
			implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], Some(p1))
			, implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], Some(p2))
		))
	}

	/**
		* Method to return a Renderer[T] where T is a 3-ary Product and which is based on a function to convert a (P1,P2,P3) into a T.
		*
		* @param construct a function (P1,P2,P3) => T, usually the apply method of a case class.
		* @tparam P1 the type of the first field of the Product type T.
		* @tparam P2 the type of the second field of the Product type T.
		* @tparam P3 the type of the third field of the Product type T.
		* @tparam T  the underlying type of the result, a Product.
		* @return a Renderer which converts Strings from a Row into the field types P1, P2 and P3 and thence into a T
		*/
	def renderer3[P1: Renderer, P2: Renderer, P3: Renderer, T <: Product : ClassTag](style: String)(construct: (P1, P2, P3) => T): Renderer[T] = new TaggedRenderer[T](style) {
		val Array(p1, p2, p3) = Reflection.extractFieldNames(implicitly[ClassTag[T]])

		override def render[U: TreeWriter](t: T, ao: Option[String]): U = implicitly[TreeWriter[U]].node(style, None, render(ao), Seq(
			implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], Some(p1))
			, implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], Some(p2))
			, implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], Some(p3))
		))
	}

	/**
		* Method to return a Renderer[T] where T is a 4-ary Product and which is based on a function to convert a (P1,P2,P3,P4) into a T.
		*
		* @param construct a function (P1,P2,P3,P4) => T, usually the apply method of a case class.
		* @tparam P1 the type of the first field of the Product type T.
		* @tparam P2 the type of the second field of the Product type T.
		* @tparam P3 the type of the second field of the Product type T.
		* @tparam P4 the type of the fourth field of the Product type T.
		* @tparam T  the underlying type of the result, a Product.
		* @return a Renderer which converts Strings from a Row into the field types P1, P2, P3 and P4 and thence into a T
		*/
	def renderer4[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, T <: Product : ClassTag](style: String)(construct: (P1, P2, P3, P4) => T): Renderer[T] = new TaggedRenderer[T](style) {
		val Array(p1, p2, p3, p4) = Reflection.extractFieldNames(implicitly[ClassTag[T]])

		override def render[U: TreeWriter](t: T, ao: Option[String]): U = implicitly[TreeWriter[U]].node(style, None, render(ao), Seq(
			implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], Some(p1))
			, implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], Some(p2))
			, implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], Some(p3))
			, implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], Some(p4))
		))
	}

	/**
		* Method to return a Renderer[T] where T is a 5-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5) into a T.
		*
		* @param construct a function (P1,P2,P3,P4,P5) => T, usually the apply method of a case class.
		* @tparam P1 the type of the first field of the Product type T.
		* @tparam P2 the type of the second field of the Product type T.
		* @tparam P3 the type of the second field of the Product type T.
		* @tparam P4 the type of the fourth field of the Product type T.
		* @tparam P5 the type of the fifth field of the Product type T.
		* @tparam T  the underlying type of the result, a Product.
		* @return a Renderer which converts Strings from a Row into the field types P1, P2, P3, P4 and P5 and thence into a T
		*/
	def renderer5[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, T <: Product : ClassTag](style: String)(construct: (P1, P2, P3, P4, P5) => T): Renderer[T] = new TaggedRenderer[T](style) {
		val Array(p1, p2, p3, p4, p5) = Reflection.extractFieldNames(implicitly[ClassTag[T]])

		override def render[U: TreeWriter](t: T, ao: Option[String]): U = implicitly[TreeWriter[U]].node(style, None, render(ao), Seq(
			implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], Some(p1))
			, implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], Some(p2))
			, implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], Some(p3))
			, implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], Some(p4))
			, implicitly[Renderer[P5]].render(t.productElement(4).asInstanceOf[P5], Some(p5))
		))
	}

	/**
		* Method to return a Renderer[T] where T is a 6-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6) into a T.
		*
		* @param construct a function (P1,P2,P3,P4,P5,P6) => T, usually the apply method of a case class.
		* @tparam P1 the type of the first field of the Product type T.
		* @tparam P2 the type of the second field of the Product type T.
		* @tparam P3 the type of the second field of the Product type T.
		* @tparam P4 the type of the fourth field of the Product type T.
		* @tparam P5 the type of the fifth field of the Product type T.
		* @tparam P6 the type of the sixth field of the Product type T.
		* @tparam T  the underlying type of the result, a Product.
		* @return a Renderer which converts Strings from a Row into the field types P1, P2, P3, P4, P5 and P6 and thence into a T
		*/
	def renderer6[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, P6: Renderer, T <: Product : ClassTag](style: String)(construct: (P1, P2, P3, P4, P5, P6) => T): Renderer[T] = new TaggedRenderer[T](style) {
		val Array(p1, p2, p3, p4, p5, p6) = Reflection.extractFieldNames(implicitly[ClassTag[T]])

		override def render[U: TreeWriter](t: T, ao: Option[String]): U = implicitly[TreeWriter[U]].node(style, None, render(ao), Seq(
			implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], Some(p1))
			, implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], Some(p2))
			, implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], Some(p3))
			, implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], Some(p4))
			, implicitly[Renderer[P5]].render(t.productElement(4).asInstanceOf[P5], Some(p5))
			, implicitly[Renderer[P6]].render(t.productElement(5).asInstanceOf[P6], Some(p6))
		))
	}

	/**
		* Method to return a Renderer[T] where T is a 7-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7) into a T.
		*
		* @param construct a function (P1,P2,P3,P4,P5,P6,P7) => T, usually the apply method of a case class.
		* @tparam P1 the type of the first field of the Product type T.
		* @tparam P2 the type of the second field of the Product type T.
		* @tparam P3 the type of the second field of the Product type T.
		* @tparam P4 the type of the fourth field of the Product type T.
		* @tparam P5 the type of the fifth field of the Product type T.
		* @tparam P6 the type of the sixth field of the Product type T.
		* @tparam P7 the type of the seventh field of the Product type T.
		* @tparam T  the underlying type of the result, a Product.
		* @return a Renderer which converts Strings from a Row into the field types P1, P2, P3, P4, P5, P6 and P7 and thence into a T
		*/
	def renderer7[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, P6: Renderer, P7: Renderer, T <: Product : ClassTag](style: String)(construct: (P1, P2, P3, P4, P5, P6, P7) => T): Renderer[T] = new TaggedRenderer[T](style) {
		val Array(p1, p2, p3, p4, p5, p6, p7) = Reflection.extractFieldNames(implicitly[ClassTag[T]])

		override def render[U: TreeWriter](t: T, ao: Option[String]): U = implicitly[TreeWriter[U]].node(style, None, render(ao), Seq(
			implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], Some(p1))
			, implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], Some(p2))
			, implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], Some(p3))
			, implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], Some(p4))
			, implicitly[Renderer[P5]].render(t.productElement(4).asInstanceOf[P5], Some(p5))
			, implicitly[Renderer[P6]].render(t.productElement(5).asInstanceOf[P6], Some(p6))
			, implicitly[Renderer[P7]].render(t.productElement(6).asInstanceOf[P7], Some(p7))
		))
	}

	/**
		* Method to return a Renderer[T] where T is a 8-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8) into a T.
		*
		* @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8) => T, usually the apply method of a case class.
		* @tparam P1 the type of the first field of the Product type T.
		* @tparam P2 the type of the second field of the Product type T.
		* @tparam P3 the type of the second field of the Product type T.
		* @tparam P4 the type of the fourth field of the Product type T.
		* @tparam P5 the type of the fifth field of the Product type T.
		* @tparam P6 the type of the sixth field of the Product type T.
		* @tparam P7 the type of the seventh field of the Product type T.
		* @tparam P8 the type of the eighth field of the Product type T.
		* @tparam T  the underlying type of the result, a Product.
		* @return a Renderer which converts Strings from a Row into the field types P1, P2, P3, P4, P5, P6, P7 and P8 and thence into a T
		*/
	def renderer8[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, P6: Renderer, P7: Renderer, P8: Renderer, T <: Product : ClassTag](style: String)(construct: (P1, P2, P3, P4, P5, P6, P7, P8) => T): Renderer[T] = new TaggedRenderer[T](style) {
		val Array(p1, p2, p3, p4, p5, p6, p7, p8) = Reflection.extractFieldNames(implicitly[ClassTag[T]])

		override def render[U: TreeWriter](t: T, ao: Option[String]): U = implicitly[TreeWriter[U]].node(style, None, render(ao), Seq(
			implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], Some(p1))
			, implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], Some(p2))
			, implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], Some(p3))
			, implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], Some(p4))
			, implicitly[Renderer[P5]].render(t.productElement(4).asInstanceOf[P5], Some(p5))
			, implicitly[Renderer[P6]].render(t.productElement(5).asInstanceOf[P6], Some(p6))
			, implicitly[Renderer[P7]].render(t.productElement(6).asInstanceOf[P7], Some(p7))
			, implicitly[Renderer[P8]].render(t.productElement(7).asInstanceOf[P8], Some(p8))
		))
	}

	/**
		* Method to return a Renderer[T] where T is a 9-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9) into a T.
		*
		* @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9) => T, usually the apply method of a case class.
		* @tparam P1 the type of the first field of the Product type T.
		* @tparam P2 the type of the second field of the Product type T.
		* @tparam P3 the type of the second field of the Product type T.
		* @tparam P4 the type of the fourth field of the Product type T.
		* @tparam P5 the type of the fifth field of the Product type T.
		* @tparam P6 the type of the sixth field of the Product type T.
		* @tparam P7 the type of the seventh field of the Product type T.
		* @tparam P8 the type of the eighth field of the Product type T.
		* @tparam P9 the type of the ninth field of the Product type T.
		* @tparam T  the underlying type of the result, a Product.
		* @return a Renderer which converts Strings from a Row into the field types P1, P2, P3, P4, P5, P6, P7, P8 and P9 and thence into a T
		*/
	def renderer9[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, P6: Renderer, P7: Renderer, P8: Renderer, P9: Renderer, T <: Product : ClassTag](style: String)(construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => T): Renderer[T] = new TaggedRenderer[T](style) {
		val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9) = Reflection.extractFieldNames(implicitly[ClassTag[T]])

		override def render[U: TreeWriter](t: T, ao: Option[String]): U = implicitly[TreeWriter[U]].node(style, None, render(ao), Seq(
			implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], Some(p1))
			, implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], Some(p2))
			, implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], Some(p3))
			, implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], Some(p4))
			, implicitly[Renderer[P5]].render(t.productElement(4).asInstanceOf[P5], Some(p5))
			, implicitly[Renderer[P6]].render(t.productElement(5).asInstanceOf[P6], Some(p6))
			, implicitly[Renderer[P7]].render(t.productElement(6).asInstanceOf[P7], Some(p7))
			, implicitly[Renderer[P8]].render(t.productElement(7).asInstanceOf[P8], Some(p8))
			, implicitly[Renderer[P9]].render(t.productElement(8).asInstanceOf[P9], Some(p9))
		))
	}

	/**
		* Method to return a Renderer[T] where T is a 10-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) into a T.
		*
		* @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10) => T, usually the apply method of a case class.
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
		* @tparam T   the underlying type of the result, a Product.
		* @return a Renderer which converts Strings from a Row into the field types P1, P2, P3, P4, P5, P6, P7, P8, P9 and P10 and thence into a T
		*/
	def renderer10[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, P6: Renderer, P7: Renderer, P8: Renderer, P9: Renderer, P10: Renderer, T <: Product : ClassTag](style: String)(construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => T): Renderer[T] = new TaggedRenderer[T](style) {
		val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10) = Reflection.extractFieldNames(implicitly[ClassTag[T]])

		override def render[U: TreeWriter](t: T, ao: Option[String]): U = implicitly[TreeWriter[U]].node(style, None, render(ao), Seq(
			implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], Some(p1))
			, implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], Some(p2))
			, implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], Some(p3))
			, implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], Some(p4))
			, implicitly[Renderer[P5]].render(t.productElement(4).asInstanceOf[P5], Some(p5))
			, implicitly[Renderer[P6]].render(t.productElement(5).asInstanceOf[P6], Some(p6))
			, implicitly[Renderer[P7]].render(t.productElement(6).asInstanceOf[P7], Some(p7))
			, implicitly[Renderer[P8]].render(t.productElement(7).asInstanceOf[P8], Some(p8))
			, implicitly[Renderer[P9]].render(t.productElement(8).asInstanceOf[P9], Some(p9))
			, implicitly[Renderer[P10]].render(t.productElement(9).asInstanceOf[P10], Some(p10))
		))
	}

	/**
		* Method to return a Renderer[T] where T is a 11-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11) into a T.
		*
		* @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11) => T, usually the apply method of a case class.
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
		* @tparam T   the underlying type of the result, a Product.
		* @return a Renderer which converts Strings from a Row into the field types P1, P2, P3, P4, P5, P6, P7, P8, P9, P10 and P11 and thence into a T
		*/
	def renderer11[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, P6: Renderer, P7: Renderer, P8: Renderer, P9: Renderer, P10: Renderer, P11: Renderer, T <: Product : ClassTag](style: String)(construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => T): Renderer[T] = new TaggedRenderer[T](style) {
		val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11) = Reflection.extractFieldNames(implicitly[ClassTag[T]])

		override def render[U: TreeWriter](t: T, ao: Option[String]): U = implicitly[TreeWriter[U]].node(style, None, render(ao), Seq(
			implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], Some(p1))
			, implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], Some(p2))
			, implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], Some(p3))
			, implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], Some(p4))
			, implicitly[Renderer[P5]].render(t.productElement(4).asInstanceOf[P5], Some(p5))
			, implicitly[Renderer[P6]].render(t.productElement(5).asInstanceOf[P6], Some(p6))
			, implicitly[Renderer[P7]].render(t.productElement(6).asInstanceOf[P7], Some(p7))
			, implicitly[Renderer[P8]].render(t.productElement(7).asInstanceOf[P8], Some(p8))
			, implicitly[Renderer[P9]].render(t.productElement(8).asInstanceOf[P9], Some(p9))
			, implicitly[Renderer[P10]].render(t.productElement(9).asInstanceOf[P10], Some(p10))
			, implicitly[Renderer[P11]].render(t.productElement(10).asInstanceOf[P11], Some(p11))
		))
	}

	/**
		* Method to return a Renderer[T] where T is a 12-ary Product and which is based on a function to convert a (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12) into a T.
		*
		* @param construct a function (P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12) => T, usually the apply method of a case class.
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
		* @tparam T   the underlying type of the result, a Product.
		* @return a Renderer which converts Strings from a Row into the field types P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11 and P12 and thence into a T
		*/
	def renderer12[P1: Renderer, P2: Renderer, P3: Renderer, P4: Renderer, P5: Renderer, P6: Renderer, P7: Renderer, P8: Renderer, P9: Renderer, P10: Renderer, P11: Renderer, P12: Renderer, T <: Product : ClassTag](style: String)(construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => T): Renderer[T] = new TaggedRenderer[T](style) {
		val Array(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12) = Reflection.extractFieldNames(implicitly[ClassTag[T]])

		override def render[U: TreeWriter](t: T, ao: Option[String]): U = implicitly[TreeWriter[U]].node(style, None, render(ao), Seq(
			implicitly[Renderer[P1]].render(t.productElement(0).asInstanceOf[P1], Some(p1))
			, implicitly[Renderer[P2]].render(t.productElement(1).asInstanceOf[P2], Some(p2))
			, implicitly[Renderer[P3]].render(t.productElement(2).asInstanceOf[P3], Some(p3))
			, implicitly[Renderer[P4]].render(t.productElement(3).asInstanceOf[P4], Some(p4))
			, implicitly[Renderer[P5]].render(t.productElement(4).asInstanceOf[P5], Some(p5))
			, implicitly[Renderer[P6]].render(t.productElement(5).asInstanceOf[P6], Some(p6))
			, implicitly[Renderer[P7]].render(t.productElement(6).asInstanceOf[P7], Some(p7))
			, implicitly[Renderer[P8]].render(t.productElement(7).asInstanceOf[P8], Some(p8))
			, implicitly[Renderer[P9]].render(t.productElement(8).asInstanceOf[P9], Some(p9))
			, implicitly[Renderer[P10]].render(t.productElement(9).asInstanceOf[P10], Some(p10))
			, implicitly[Renderer[P11]].render(t.productElement(10).asInstanceOf[P11], Some(p11))
			, implicitly[Renderer[P12]].render(t.productElement(11).asInstanceOf[P12], Some(p12))
		))
	}
}

/**
	* This companion object comprises Renderer[T] objects which represent conversions that are fixed,
	* i.e. they don't depend on some other parameter such as the formatter in DateTime conversions.
	*/
object Renderers {

}
