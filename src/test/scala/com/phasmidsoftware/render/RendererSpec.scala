/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.render.tag.{Attribute, HTML}
import com.phasmidsoftware.table.{Header, Indexed, TableWithHeader, TableWithoutHeader}
import org.scalatest.{FlatSpec, Matchers}

class RendererSpec extends FlatSpec with Matchers {

	case class Complex(r: Double, i: Double)

	case class SimpleHTML(tag: String, content: Option[String], attributes: Map[String, String], hs: Seq[SimpleHTML])

	object SimpleHTML {
		def apply(x: String, hs: Seq[SimpleHTML]): SimpleHTML = apply(x, None, Map.empty, hs)

		def apply(x: String, ao: Option[String]): SimpleHTML = apply(x, ao, Map.empty, Nil)

		def apply(x: String): SimpleHTML = apply(x, None)
	}

	case class Complicated(name: String, count: Int, open: Boolean, maybePhone: Option[Long], aliases: Seq[String])

	object Complex1 extends Renderers {
		implicit val complexRenderer: Renderer[Complex] = renderer2("complex")(Complex.apply)

		trait TreeWriterString$ extends TreeWriter[String] {

			override def addChild(parent: String, child: String): String = parent + ", " + child

			def node(tag: String, content: Option[String], attributes: Map[String, String], children: Seq[String]): String = s"""<$tag>: "${content.getOrElse("")}" ${attributes.mkString("(", ",", ")")} ${children.mkString("[", ",", "]")} """
		}

		implicit object TreeWriterString$ extends TreeWriterString$
	}

	object Complex2 extends Renderers {
		implicit val valueRenderer: Renderer[Double] = renderer("td")
		implicit val complexRenderer: Renderer[Complex] = renderer2("span")(Complex)
		implicit val indexedRenderer: Renderer[Indexed[Complex]] = indexedRenderer[Complex]("tr", "th", Map())
		//		val rowsRenderer: Renderer[Seq[Indexed[Complex]]] = sequenceRenderer[Indexed[Complex]]("span")


		import HTML._
		trait TreeWriterHTML$ extends TreeWriter[HTML] {
			def addChild(parent: HTML, child: HTML): HTML = parent match {
				case HTML(t, co, as, hs) => HTML(t, co, as, hs :+ child)
			}

			def node(tag: String, content: Option[String], attributes: Map[String, String], children: Seq[HTML]): HTML = HTML(tag, attributes.toSeq.map(kv => Attribute(kv)), content, children)
		}

		implicit object TreeWriterHTML$ extends TreeWriterHTML$
	}

	object Complex3 extends Renderers {
		implicit val valueRenderer: Renderer[Double] = renderer("td")
		implicit val complexRenderer: Renderer[Complex] = renderer2("tr")(Complex)
		//		val rowsRenderer: Renderer[Seq[Indexed[Complex]]] = sequenceRenderer[Indexed[Complex]]("span")


		import HTML._
		trait TreeWriterHTML$ extends TreeWriter[HTML] {
			def addChild(parent: HTML, child: HTML): HTML = parent match {
				case HTML(t, co, as, hs) => HTML(t, co, as, hs :+ child)
			}

			def node(tag: String, content: Option[String], attributes: Map[String, String], children: Seq[HTML]): HTML = HTML(tag, attributes.toSeq.map(kv => Attribute(kv)), content, children)
		}

		implicit object TreeWriterHTML$ extends TreeWriterHTML$
	}

	object Complicated extends Renderers {
		implicit val elementRenderer: Renderer[String] = renderer("element")
		implicit val optionLongRenderer: Renderer[Option[Long]] = optionRenderer
		implicit val sequenceStringRenderer: Renderer[Seq[String]] = sequenceRenderer("")
		implicit val complicatedRenderer: Renderer[Complicated] = renderer5("x")(Complicated.apply)

		trait TreeWriterHTML$ extends TreeWriter[SimpleHTML] {
			def addChild(parent: SimpleHTML, child: SimpleHTML): SimpleHTML = parent match {
				case SimpleHTML(t, co, as, hs) => SimpleHTML(t, co, as, hs :+ child)
			}

			def node(tag: String, content: Option[String], attributes: Map[String, String], children: Seq[SimpleHTML]): SimpleHTML = SimpleHTML(tag, content, attributes, children)
		}

		implicit object TreeWriterHTML$ extends TreeWriterHTML$
	}

	behavior of "Renderer.render"

	it should "render Complex as sequence of numbers" in {
		import Complex1._
		val z = Complex(0, 1)
		Renderer.render(z) shouldBe "<complex>: \"\" () [<>: \"0.0\" (name -> r) [] ,<>: \"1.0\" (name -> i) [] ] "
	}

	it should "render Complex as an HTML" in {
		import Complex3._
		val z = Complex(0, 1)
		import HTML._
		Renderer.render(z) shouldBe HTML("tr", Nil, None, List(HTML("td", Seq(Attribute("name" -> "r")), Some("0.0"), Nil), HTML("td", Seq(Attribute("name" -> "i")), Some("1.0"), Nil)))
	}

	it should "render Complicated as an SimpleHTML" in {
		import Complicated._
		val z = Complicated("strange", 42, open = false, Some(6175551234L), Seq("Tom", "Dick", "Harry"))
		Renderer.render(z) shouldBe SimpleHTML("x", None, Map.empty, List(SimpleHTML("element", Some("strange"), Map("name" -> "name"), List()), SimpleHTML("", Some("42"), Map("name" -> "count"), List()), SimpleHTML("", Some("false"), Map("name" -> "open"), List()), SimpleHTML("", None, Map("name" -> "maybePhone"), List(SimpleHTML("", Some("6175551234"), Map.empty, List()))), SimpleHTML("", None, Map("name" -> "aliases"), List(SimpleHTML("element", Some("Tom"), Map.empty, List()), SimpleHTML("element", Some("Dick"), Map.empty, List()), SimpleHTML("element", Some("Harry"), Map.empty, List())))))

		//		Renderer.render(z, "Complicated") shouldBe
		//			Renderer.render(z) shouldBe SimpleHTML("x", None, Map("name"->"Complicated"), List(SimpleHTML("element", Some("strange"), Map("name"->"name"), List()), SimpleHTML("", Some("42"), Map("name"->"count"), List()), SimpleHTML("", Some("false"), Map("name"->"open"), List()), SimpleHTML("", None, Map("name"->"maybePhone"), List(SimpleHTML("", Some("6175551234"), Map.empty, List()))), SimpleHTML("", None, Map("name"->"aliases"), List(SimpleHTML("element", Some("Tom"), Map.empty, List()), SimpleHTML("element", Some("Dick"), Map.empty, List()), SimpleHTML("element", Some("Harry"), Map.empty, List())))))
	}

	it should "render a table of Complexes in HTML without a header" in {
		import Complex2._
		val table = TableWithoutHeader(Seq(Complex(0, 1), Complex(-1, 0)))
		val h = table.render("table", Map("border" -> "1"))
		println(h)
	}

	it should "render a table of Complexes in HTML with a header" in {
		import Complex2._
		val table = TableWithHeader(Seq(Complex(0, 1), Complex(-1, 0)), Header(Seq("real", "imaginary")))
		val h = table.render("table", Map("border" -> "1"))
		println(h)
	}

}
