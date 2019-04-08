/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import org.scalatest.{FlatSpec, Matchers}

class TreeWriterSpec extends FlatSpec with Matchers {

	case class HTML(x: String, ao: Option[String], attrs: Map[String, String], hs: Seq[HTML])

	object HTML {
		def apply(x: String): HTML = apply(x, None, Map.empty)

		def apply(x: String, a: String): HTML = apply(x, Some(a), Map.empty, Nil)

		def apply(x: String, ao: Option[String], as: Map[String, String]): HTML = apply(x, ao, as, Nil)

		def apply(x: String, hs: Seq[HTML]): HTML = apply(x, None, Map.empty, hs)

		trait HTMLTreeWriter extends TreeWriter[HTML] {

			def addChild(parent: HTML, child: HTML): HTML = parent match {
				case HTML(t, co, as, hs) => HTML(t, co, as, hs :+ child)
			}

			def node(tag: String, content: Option[String], attributes: Map[String, String], children: Seq[HTML]): HTML =
				HTML(tag, content map (_.toString), attributes, children)
		}

		implicit object HTMLTreeWriter extends HTMLTreeWriter

	}

	behavior of "TreeWriter"

	import HTML._

	it should "implement node correctly for 1" in {
		implicitly[TreeWriter[HTML]].node("1", None, Map.empty) shouldBe HTML("1")
		implicitly[TreeWriter[HTML]].node("1", None, Map("name" -> "x")) shouldBe HTML("1", None, Map("name" -> "x"))
	}


}
