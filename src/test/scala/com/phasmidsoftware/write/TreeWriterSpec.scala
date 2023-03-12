/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.write

import org.scalatest.flatspec
import org.scalatest.matchers.should

class TreeWriterSpec extends flatspec.AnyFlatSpec with should.Matchers {

  case class HTML(x: String, ao: Option[String], attrs: Map[String, String], hs: Seq[HTML])

  object HTML {
    def apply(x: String): HTML = apply(x, None, Map.empty)

    def apply(x: String, ao: Option[String], as: Map[String, String]): HTML = apply(x, ao, as, Nil)

    def apply(x: String, a: String): HTML = apply(x, Some(a), Map.empty, Nil)

    def apply(x: String, hs: Seq[HTML]): HTML = apply(x, None, Map.empty, hs)

    trait HTMLTreeWriter extends TreeWriter[HTML] {
      def evaluate(node: Node): HTML = HTML(node.style, node.content map identity, node.attributes, node.children.toSeq map evaluate)
    }

    implicit object HTMLTreeWriter extends HTMLTreeWriter

  }

  behavior of "TreeWriter"

  import HTML._

  it should "implement node correctly for 1" in {
    implicitly[TreeWriter[HTML]].evaluate(Node("1")) shouldBe HTML("1")
    implicitly[TreeWriter[HTML]].evaluate(Node("1", Map("name" -> "x"))) shouldBe HTML("1", None, Map("name" -> "x"))
  }


}
