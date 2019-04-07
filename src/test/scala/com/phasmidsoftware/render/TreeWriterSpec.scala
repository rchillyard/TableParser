/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import org.scalatest.{FlatSpec, Matchers}

class TreeWriterSpec extends FlatSpec with Matchers {

	case class HTML(x: String, ao: Option[String], attrs: Seq[String], hs: Seq[HTML])

	object HTML {
		def apply(x: String): HTML = apply(x, None, Nil, Nil)

		def apply(x: String, a: String): HTML = apply(x, Some(a), Nil, Nil)

		def apply(x: String, hs: Seq[HTML]): HTML = apply(x, None, Nil, hs)

		trait HTMLTreeWriter extends TreeWriter[HTML] {

			override def node(tag: String, content: Option[String], attributes: Seq[String], children: Seq[HTML]): HTML =
				HTML(tag, content map (_.toString), attributes, children)
		}

		implicit object HTMLTreeWriter extends HTMLTreeWriter

	}

	behavior of "TreeWriter"

	import HTML._

	it should "implement node correctly for 1" in {
		implicitly[TreeWriter[HTML]].node("1", None, Nil, Nil) shouldBe HTML("1")
		implicitly[TreeWriter[HTML]].node("1", None, Seq("x"), Nil) shouldBe HTML("1", None, Seq("x"), Nil)
	}


}
