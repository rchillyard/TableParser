/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.util.Success

class ParseableSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "Parseable"

  it should "parse Hello" in {
    implicitly[Parseable[String]].parse("Hello") shouldBe Success("Hello")
  }

  it should "parse true" in {
    implicitly[Parseable[Boolean]].parse("true") shouldBe Success(true)
  }

  it should "parse 7" in {
    implicitly[Parseable[Byte]].parse("7") shouldBe Success(Byte.box(7))
  }

  it should "parse 9" in {
    implicitly[Parseable[Short]].parse("99") shouldBe Success(Short.box(99))
  }

  it should "parse 99" in {
    implicitly[Parseable[Int]].parse("99") shouldBe Success(99)
  }

  it should "parse 999999999" in {
    implicitly[Parseable[Long]].parse("999999999") shouldBe Success(999999999L)
  }

  it should "parse 99999999999999999" in {
    implicitly[Parseable[BigInt]].parse("99999999999999999") shouldBe Success(BigInt("99999999999999999"))
  }

  it should "parse 3.1415927f" in {
    implicitly[Parseable[Float]].parse("3.1415927") shouldBe Success(3.1415927f)
  }

  it should "parse 3.1415927" in {
    implicitly[Parseable[Double]].parse("3.1415927") shouldBe Success(3.1415927)
  }

  it should "parse 9.9999999999999999" in {
    implicitly[Parseable[BigDecimal]].parse("9.9999999999999999") shouldBe Success(BigDecimal("9.9999999999999999"))
  }

  it should "parse {1,2,3}" in {
    implicitly[Parseable[StringList]].parse("{1,2,3}") shouldBe Success(List("1", "2", "3"))
  }

  behavior of "ListParser"
  it should "parse a list" in {
    val p = ListParser
    p.parseAll(p.list, "{1,2,3}") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.list, "{1,2-3,3}") should matchPattern { case p.Success(_, _) => }
    p.parseAll(p.list, "1") should matchPattern { case p.Success(_, _) => }
  }

  behavior of "OptionParser"
  it should "parse an optional String value" in {
    import ParseableOption._
    implicitly[Parseable[Option[String]]].parse("Hello") shouldBe Success(Some("Hello"))
    implicitly[Parseable[Option[String]]].parse("") shouldBe Success(None)
  }

  it should "parse an optional Int value" in {
    import ParseableOption._
    implicitly[Parseable[Option[Int]]].parse("1") shouldBe Success(Some(1))
    implicitly[Parseable[Option[Int]]].parse("") shouldBe Success(None)
  }

  // NOTE: please leave this comment here in order to enable testing of the implicitNotFound message
  //  class NotACaseCLass(val x: Int)
  //
  //  behavior of "non-case-classes"
  //  it should "cause a warning message in the compiler" in {
  //    implicitly[Parseable[NotACaseCLass]].parse(1) shouldBe 1
  //  }
}
