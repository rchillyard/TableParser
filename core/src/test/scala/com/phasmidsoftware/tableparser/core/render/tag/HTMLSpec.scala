package com.phasmidsoftware.tableparser.core.render.tag

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class HTMLSpec extends AnyFlatSpec with should.Matchers:

  behavior of "HTML"

  it should "make a tag with a name" in:
    val tag = HTML("td")
    tag.name shouldBe "td"
    tag.attributes shouldBe Nil
    tag.content shouldBe None
    tag.tags shouldBe Nil
    tag.toString shouldBe "\n<td></td>"

  it should "make a span if the name is empty" in:
    HTML("").name shouldBe "span"

  it should "make a tag with content" in:
    HTML("td", Some("42")).toString shouldBe "\n<td>42</td>"

  it should "make a tag with attributes" in:
    val tag = HTML("table", Map("border" -> "1"))
    tag.attributes shouldBe Seq(Attribute("border", "1"))
    tag.toString shouldBe "\n<table border=\"1\"></table>"

  it should "make a tag with attributes and content" in:
    HTML("th", Map("class" -> "x"), Some("name")).toString shouldBe "\n<th class=\"x\">name</th>"

  it should "add a child tag" in:
    val tag = HTML("tr") :+ HTML("td", Some("1")) :+ HTML("td", Some("2"))
    tag.tags.size shouldBe 2
    tag.toString shouldBe "\n<tr>\n<td>1</td>\n<td>2</td></tr>"

  behavior of "Attribute"

  it should "render as key=\"value\"" in:
    Attribute("k", "v").toString shouldBe "k=\"v\""

  it should "be made from a tuple or a Map" in:
    Attribute("k" -> "v") shouldBe Attribute("k", "v")
    Attribute.mapToAttributes(Map("a" -> "1", "b" -> "2")) shouldBe Seq(Attribute("a", "1"), Attribute("b", "2"))
