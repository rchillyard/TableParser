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

}
