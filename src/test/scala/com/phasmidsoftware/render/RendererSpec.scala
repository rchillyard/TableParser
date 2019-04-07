/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import org.scalatest.{FlatSpec, Matchers}

class RendererSpec extends FlatSpec with Matchers {

	case class Complex(r: Double, i: Double)

	case class HTML(tag: String, content: Option[String], attributes: Seq[String], hs: Seq[HTML])

	object HTML {
		def apply(x: String, hs: Seq[HTML]): HTML = apply(x, None, Nil, hs)

		def apply(x: String, ao: Option[String]): HTML = apply(x, ao, Nil, Nil)

		def apply(x: String): HTML = apply(x, None)
	}

	case class Complicated(name: String, count: Int, open: Boolean, maybePhone: Option[Long], aliases: Seq[String])

	object Complex1 extends Renderers {
		implicit val complexRenderer: Renderer[Complex] = renderer2("complex")(Complex.apply)

		trait TreeWriterString$ extends TreeWriter[String] {

			override def addChild(parent: String, child: String): String = parent + ", " + child

			def node(tag: String, content: Option[String], attributes: Seq[String], children: Seq[String]): String = s"""<$tag>: "${content.getOrElse("")}" ${attributes.mkString("(", ",", ")")} ${children.mkString("[", ",", "]")} """
		}

		implicit object TreeWriterString$ extends TreeWriterString$
	}

	object Complex2 extends Renderers {
		implicit val complexRenderer: Renderer[Complex] = renderer2("complex")(Complex.apply)

		trait TreeWriterHTML$ extends TreeWriter[HTML] {
			def addChild(parent: HTML, child: HTML): HTML = parent match {
				case HTML(t, co, as, hs) => HTML(t, co, as, hs :+ child)
			}

			def node(tag: String, content: Option[String], attributes: Seq[String], children: Seq[HTML]): HTML = HTML(tag, content, attributes, children)
		}

		implicit object TreeWriterHTML$ extends TreeWriterHTML$
	}

	object Complicated extends Renderers {
		implicit val elementRenderer: Renderer[String] = renderer("element")
		implicit val optionLongRenderer: Renderer[Option[Long]] = optionRenderer
		implicit val sequenceStringRenderer: Renderer[Seq[String]] = sequenceRenderer("")
		implicit val complicatedRenderer: Renderer[Complicated] = renderer5("x")(Complicated.apply)

		trait TreeWriterHTML$ extends TreeWriter[HTML] {
			def addChild(parent: HTML, child: HTML): HTML = parent match {
				case HTML(t, co, as, hs) => HTML(t, co, as, hs :+ child)
			}

			def node(tag: String, content: Option[String], attributes: Seq[String], children: Seq[HTML]): HTML = HTML(tag, content, attributes, children)
		}

		implicit object TreeWriterHTML$ extends TreeWriterHTML$
	}

	behavior of "Renderer.render"

	it should "render Complex as sequence of numbers" in {
		import Complex1._
		val z = Complex(0, 1)
		Renderer.render(z) shouldBe "<complex>: \"\" () [<>: \"0.0\" (r) [] ,<>: \"1.0\" (i) [] ] "
	}

	it should "render Complex as an HTML" in {
		import Complex2._
		val z = Complex(0, 1)
		Renderer.render(z) shouldBe HTML("complex", None, List(), List(HTML("", Some("0.0"), List("r"), List()), HTML("", Some("1.0"), List("i"), List())))
	}

	it should "render Complicated as an HTML" in {
		import Complicated._
		val z = Complicated("strange", 42, open = false, Some(6175551234L), Seq("Tom", "Dick", "Harry"))
		Renderer.render(z) shouldBe HTML("x", None, List(), List(HTML("element", Some("strange"), List("name"), List()), HTML("", Some("42"), List("count"), List()), HTML("", Some("false"), List("open"), List()), HTML("", None, List("maybePhone"), List(HTML("", Some("6175551234"), List(), List()))), HTML("", None, List("aliases"), List(HTML("element", Some("Tom"), List(), List()), HTML("element", Some("Dick"), List(), List()), HTML("element", Some("Harry"), List(), List())))))
		Renderer.render(z, "Complicated") shouldBe
			HTML("x", None, List("Complicated"), List(HTML("element", Some("strange"), List("name"), List()), HTML("", Some("42"), List("count"), List()), HTML("", Some("false"), List("open"), List()), HTML("", None, List("maybePhone"), List(HTML("", Some("6175551234"), List(), List()))), HTML("", None, List("aliases"), List(HTML("element", Some("Tom"), List(), List()), HTML("element", Some("Dick"), List(), List()), HTML("element", Some("Harry"), List(), List())))))
	}

}
