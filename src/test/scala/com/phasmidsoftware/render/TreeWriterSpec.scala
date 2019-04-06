/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import org.scalatest.{FlatSpec, Matchers}

class TreeWriterSpec extends FlatSpec with Matchers {

	case class HTML(x: String, ao: Option[String], hs: Seq[HTML])

	object HTML {
		def apply(x: String): HTML = apply(x, None, Nil)

		def apply(x: String, a: String): HTML = apply(x, Some(a), Nil)

		def apply(x: String, hs: Seq[HTML]): HTML = apply(x, None, hs)

		trait HTMLTreeWriter extends TreeWriter[HTML] {
			/**
				* Create a new sub-tree with children defined by the sequence us.
				*
				* @param us a sequence of instances of U.
				* @param ao an optional attribute name.
				* @return a new instance of U (i.e. a node).
				*/
			override def subtree(us: Seq[HTML], ao: Option[String]): HTML = HTML("", ao, us)

			/**
				* Create a new node which may be a sub-tree or a leaf, depending on the nature of type T
				*
				* @param t  an instance of T.
				* @param ao an optional attribute name.
				* @tparam T the input type that is to be rendered as a node.
				* @return a new instance of U (i.e. a node).
				*/
			override def node[T: Renderer](t: T, ao: Option[String]): HTML = implicitly[Renderer[T]].render(t, ao)(this)

			/**
				* Create a new leaf node.
				* TODO figure out whether t should be an Any or a T.
				*
				* @param t  an instance of T.
				* @param ao an optional attribute name.
				* @return a new instance of U (i.e. a node).
				*/
			override def leaf(t: Any, ao: Option[String]): HTML = {
				val z = ao.toSeq.map(_ + ": ").mkString("")
				HTML(z + t.toString)
			}
		}

		implicit object HTMLTreeWriter extends HTMLTreeWriter

	}

	behavior of "TreeWriter"

	import HTML._

	it should "implement node correctly for 1" in {
		implicitly[TreeWriter[HTML]].node(1, None) shouldBe HTML("1")
		implicitly[TreeWriter[HTML]].node(1, Some("x")) shouldBe HTML("x: 1")
	}

	it should "implement leaf correctly for 1" in {
		implicitly[TreeWriter[HTML]].leaf(1, None) shouldBe HTML("1")
		implicitly[TreeWriter[HTML]].leaf(1, Some("x")) shouldBe HTML("x: 1")
	}

	it should "implement subtree correctly for 1" in {
		implicitly[TreeWriter[HTML]].subtree(Seq(HTML("1")), None) shouldBe HTML("", Seq(HTML("1")))
		implicitly[TreeWriter[HTML]].subtree(Seq(HTML("1")), Some("x")) shouldBe HTML("", Some("x"), Seq(HTML("1")))
	}

}
