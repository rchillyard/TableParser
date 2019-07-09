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

  case class Complicated(name: String, count: Int, open: Boolean, maybePhone: Option[Long], aliases: Seq[String])

  object SimpleHTML {
    def apply(x: String, hs: Seq[SimpleHTML]): SimpleHTML = apply(x, None, Map.empty, hs)

    def apply(x: String): SimpleHTML = apply(x, None)

    def apply(x: String, ao: Option[String]): SimpleHTML = apply(x, ao, Map.empty, Nil)
  }

  object Complex1 extends Renderers {
    implicit val complexRenderer: Renderer[Complex] = renderer2("complex")(Complex.apply)

    trait TreeWriterString$ extends TreeWriter[String] {
      def evaluate(node: Node): String = s"""<${node.style}>: "${node.content.getOrElse("")}" ${node.attributes.mkString("(", ",", ")")} ${node.children.map(evaluate).mkString("[", ",", "]")} """
    }

    implicit object TreeWriterString$ extends TreeWriterString$

  }

  object Complex2 extends Renderers {
    implicit val valueRenderer: Renderer[Double] = renderer("td")
    implicit val complexRenderer: Renderer[Complex] = renderer2("")(Complex)
    implicit val indexedRenderer: Renderer[Indexed[Complex]] = indexedRenderer[Complex]("tr", "th")

    import HTML._

    trait TreeWriterHTML$ extends TreeWriter[HTML] {
      def evaluate(node: Node): HTML = HTML(node.style, node.attributes.toSeq.map(kv => Attribute(kv)), node.content, node.children map evaluate)
    }

    implicit object TreeWriterHTML$ extends TreeWriterHTML$

  }

  object Complex3 extends Renderers {
    implicit val valueRenderer: Renderer[Double] = renderer("td")
    implicit val complexRenderer: Renderer[Complex] = renderer2("tr")(Complex)


    import HTML._

    trait TreeWriterHTML$ extends TreeWriter[HTML] {
      def evaluate(node: Node): HTML = HTML(node.style, node.attributes.toSeq.map(kv => Attribute(kv)), node.content, node.children map evaluate)
    }

    implicit object TreeWriterHTML$ extends TreeWriterHTML$

  }

  object Complicated extends Renderers {
    implicit val elementRenderer: Renderer[String] = renderer("element")
    implicit val optionLongRenderer: Renderer[Option[Long]] = optionRenderer("", Map())
    implicit val sequenceStringRenderer: Renderer[Seq[String]] = sequenceRenderer("")
    implicit val complicatedRenderer: Renderer[Complicated] = renderer5("x")(Complicated.apply)

    trait TreeWriterHTML$ extends TreeWriter[SimpleHTML] {
      def evaluate(node: Node): SimpleHTML = SimpleHTML(node.style, node.content, node.attributes, node.children map evaluate)
    }

    implicit object TreeWriterHTML$ extends TreeWriterHTML$

  }

  behavior of "Renderer.render"

  it should "render Complex as sequence of numbers" in {
    import Complex1._
    val z = Complex(0, 1)
    val node = implicitly[Renderer[Complex]].render(z)
    implicitly[TreeWriter[String]].evaluate(node) shouldBe "<complex>: \"\" () [<>: \"0.0\" (name -> r) [] ,<>: \"1.0\" (name -> i) [] ] "
  }

  it should "render Complex as an HTML" in {
    import Complex3._
    val z = Complex(0, 1)
    import HTML._
    val node = implicitly[Renderer[Complex]].render(z)
    implicitly[TreeWriter[HTML]].evaluate(node) shouldBe HTML("tr", Nil, None, List(HTML("td", Seq(Attribute("name" -> "r")), Some("0.0"), Nil), HTML("td", Seq(Attribute("name" -> "i")), Some("1.0"), Nil)))
  }

  it should "render Complicated as an SimpleHTML" in {
    import Complicated._
    val z = Complicated("strange", 42, open = false, Some(6175551234L), Seq("Tom", "Dick", "Harry"))
    val node = implicitly[Renderer[Complicated]].render(z)
    implicitly[TreeWriter[SimpleHTML]].evaluate(node) shouldBe SimpleHTML("x", None, Map.empty, List(SimpleHTML("element", Some("strange"), Map("name" -> "name"), List()), SimpleHTML("", Some("42"), Map("name" -> "count"), List()), SimpleHTML("", Some("false"), Map("name" -> "open"), List()), SimpleHTML("", None, Map("name" -> "maybePhone"), List(SimpleHTML("", Some("6175551234"), Map.empty, List()))), SimpleHTML("", None, Map("name" -> "aliases"), List(SimpleHTML("element", Some("Tom"), Map.empty, List()), SimpleHTML("element", Some("Dick"), Map.empty, List()), SimpleHTML("element", Some("Harry"), Map.empty, List())))))
  }

  it should "render a table of Complexes in HTML without a header" in {
    import Complex1._
    val table = TableWithoutHeader(Seq(Complex(0, 1), Complex(-1, 0)))
    val h = table.render("table", Map("border" -> "1"))
    h.toString shouldBe "<table>: \"\" (border -> 1) [<>: \"\" () [] ,<tbody>: \"\" () [<complex>: \"\" () [<>: \"0.0\" (name -> r) [] ,<>: \"1.0\" (name -> i) [] ] ,<complex>: \"\" () [<>: \"-1.0\" (name -> r) [] ,<>: \"0.0\" (name -> i) [] ] ] ] "
  }

  it should "render a table of sequenced Complexes in HTML without a header" in {
    import Complex2._
    val table = TableWithoutHeader(Seq(Complex(0, 1), Complex(-1, 0)))
    val h = table.renderSequenced("table", Map("border" -> "1"))
    h.toString shouldBe "\n<table border=\"1\">\n<tbody>\n<tr>\n<th>0</th>\n<td name=\"r\">0.0</td>\n<td name=\"i\">1.0</td></tr>\n<tr>\n<th>1</th>\n<td name=\"r\">-1.0</td>\n<td name=\"i\">0.0</td></tr></tbody></table>"
  }

  it should "render a table of sequenced Complexes in HTML with a header" in {
    import Complex2._
    val table = TableWithHeader(Seq(Complex(0, 1), Complex(-1, 0)), Header(Seq("real", "imaginary")))
    val h = table.renderSequenced("table", Map("border" -> "1"))
    h.toString shouldBe
      """
<table border="1">
<thead>
<tr>
<th></th>
<th>real</th>
<th>imaginary</th></tr></thead>
<tbody>
<tr>
<th>0</th>
<td name="r">0.0</td>
<td name="i">1.0</td></tr>
<tr>
<th>1</th>
<td name="r">-1.0</td>
<td name="i">0.0</td></tr></tbody></table>"""
  }

}
