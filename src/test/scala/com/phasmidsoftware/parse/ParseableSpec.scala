/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import org.scalatest.{FlatSpec, Matchers}

class ParseableSpec extends FlatSpec with Matchers {

	behavior of "Parseable"

	it should "parse Hello" in {
		implicitly[Parseable[String]].parse("Hello") shouldBe "Hello"
	}

	it should "parse true" in {
		implicitly[Parseable[Boolean]].parse("true") shouldBe true
	}

	it should "parse 99" in {
		implicitly[Parseable[Int]].parse("99") shouldBe 99
	}

	it should "parse 999999999" in {
		implicitly[Parseable[Long]].parse("999999999") shouldBe 999999999L
	}

	it should "parse 99999999999999999" in {
		implicitly[Parseable[BigInt]].parse("99999999999999999") shouldBe BigInt("99999999999999999")
	}

	it should "parse 3.1415927" in {
		implicitly[Parseable[Double]].parse("3.1415927") shouldBe 3.1415927
	}

	it should "parse {1,2,3}" in {
		implicitly[Parseable[StringList]].parse("{1,2,3}") shouldBe List("1", "2", "3")
	}


	behavior of "ListParser"
	it should "parse a list" in {
		val p = new ListParser
		p.parseAll(p.list, "{1,2,3}") should matchPattern { case p.Success(_, _) => }
		p.parseAll(p.list, "{1,2-3,3}") should matchPattern { case p.Success(_, _) => }
		p.parseAll(p.list, "1") should matchPattern { case p.Success(_, _) => }
	}

	behavior of "OptionParser"
	it should "parse an optional String value" in {
		import ParseableOption._
		implicitly[Parseable[Option[String]]].parse("Hello") shouldBe Some("Hello")
		implicitly[Parseable[Option[String]]].parse("") shouldBe None
	}

	it should "parse an optional Int value" in {
		import ParseableOption._
		implicitly[Parseable[Option[Int]]].parse("1") shouldBe Some(1)
		implicitly[Parseable[Option[Int]]].parse("") shouldBe None
	}

	// NOTE: please leave this comment here in order to enable testing of the implicitNotFound message
	//  class NotACaseCLass(val x: Int)
	//
	//  behavior of "non-case-classes"
	//  it should "cause a warning message in the compiler" in {
	//    implicitly[Parseable[NotACaseCLass]].parse(1) shouldBe 1
	//  }
}
