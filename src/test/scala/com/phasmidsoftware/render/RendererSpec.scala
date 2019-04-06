/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import org.scalatest.{FlatSpec, Matchers}

class RendererSpec extends FlatSpec with Matchers {

	case class Complex(r: Double, i: Double)

	case class HTML(x: String, ao: Option[String], hs: Seq[HTML])

	object HTML {
		def apply(x: String, hs: Seq[HTML]): HTML = apply(x, None, hs)

		def apply(x: String, ao: Option[String]): HTML = apply(x, ao, Nil)

		def apply(x: String): HTML = apply(x, None)
	}

	case class Complicated(name: String, count: Int, open: Boolean, maybePhone: Option[Long], aliases: Seq[String])

	object Complex1 extends Renderers {
		implicit val complexRenderer: Renderer[Complex] = renderer2(Complex.apply)

		trait TreeWriterString$ extends TreeWriter[String] {
			def subtree(us: Seq[String], ao: Option[String]): String = us mkString("(", ",", ")")

			def node[T: Renderer](t: T, ao: Option[String]): String = implicitly[Renderer[T]].render(t)(this)

			def leaf(t: Any, ao: Option[String]): String = t.toString

		}

		implicit object TreeWriterString$ extends TreeWriterString$

	}

	object Complex2 extends Renderers {
		implicit val complexRenderer: Renderer[Complex] = renderer2(Complex.apply)

		trait TreeWriterHTML$ extends TreeWriter[HTML] {
			def subtree(us: Seq[HTML], ao: Option[String]): HTML = HTML("", ao, us)

			def node[T: Renderer](t: T, ao: Option[String]): HTML = implicitly[Renderer[T]].render(t, ao)(this)

			def leaf(t: Any, ao: Option[String]): HTML = HTML(t.toString, ao)

		}

		implicit object TreeWriterHTML$ extends TreeWriterHTML$

	}

	object Complicated extends Renderers {
		implicit val optionLongRenderer: Renderer[Option[Long]] = optionRenderer
		implicit val sequenceStringRenderer: Renderer[Seq[String]] = sequenceRenderer
		implicit val complicatedRenderer: Renderer[Complicated] = renderer5(Complicated.apply)

		trait TreeWriterHTML$ extends TreeWriter[HTML] {
			def subtree(us: Seq[HTML], ao: Option[String]): HTML = HTML("", ao, us)

			def node[T: Renderer](t: T, ao: Option[String]): HTML = implicitly[Renderer[T]].render(t, ao)(this)

			def leaf(t: Any, ao: Option[String]): HTML = HTML(t.toString, ao)
		}

		implicit object TreeWriterHTML$ extends TreeWriterHTML$

	}

	behavior of "Renderer.render"

	it should "render Complex as sequence of numbers" in {
		import Complex1._
		val z = Complex(0, 1)
		Renderer.render(z) shouldBe "(0.0,1.0)"
	}

	it should "render Complex as an HTML" in {
		import Complex2._
		val z = Complex(0, 1)
		Renderer.render(z) shouldBe HTML("", Seq(HTML("0.0", Some("r")), HTML("1.0", Some("i"))))
	}

	it should "render Complicated as an HTML" in {
		import Complicated._
		val z = Complicated("strange", 42, open = false, Some(6175551234L), Seq("Tom", "Dick", "Harry"))
		Renderer.render(z) shouldBe HTML("", Seq(HTML("strange", Some("name")), HTML("42", Some("count")), HTML("false", Some("open")), HTML("", Some("maybePhone"), List(HTML("6175551234"))), HTML("", Some("aliases"), List(HTML("Tom"), HTML("Dick"), HTML("Harry")))))
		Renderer.render(z, "Complicated") shouldBe HTML("", Some("Complicated"), Seq(HTML("strange", Some("name")), HTML("42", Some("count")), HTML("false", Some("open")), HTML("", Some("maybePhone"), List(HTML("6175551234"))), HTML("", Some("aliases"), List(HTML("Tom"), HTML("Dick"), HTML("Harry")))))
	}

}
