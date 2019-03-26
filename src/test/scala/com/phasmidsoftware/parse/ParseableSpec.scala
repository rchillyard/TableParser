package com.phasmidsoftware.parse

import org.scalatest.{FlatSpec, Matchers}

class ParseableSpec extends FlatSpec with Matchers {

  behavior of "Parseable"

  it should "parse true" in {
    implicitly[Parseable[Boolean]].parse("true") shouldBe true
  }

  it should "parse 99" in {
    implicitly[Parseable[Int]].parse("99") shouldBe 99
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
  }

  behavior of "OptionParser"
  it should "parse an optional value" in {
    import ParseableOption._
    implicitly[Parseable[Option[Int]]].parse("1") shouldBe Some(1)
    implicitly[Parseable[Option[Int]]].parse("") shouldBe None
  }
}
