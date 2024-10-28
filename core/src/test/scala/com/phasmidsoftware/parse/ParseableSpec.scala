/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import java.io.File
import java.net.URL
import org.joda.time.LocalDate
import org.scalatest.flatspec
import org.scalatest.matchers.should
import scala.util.{Failure, Success}

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

  it should "parse an optional Boolean value" in {
    import ParseableOption._
    implicitly[Parseable[Option[Boolean]]].parse("true") shouldBe Success(Some(true))
    implicitly[Parseable[Option[Boolean]]].parse("") shouldBe Success(None)
  }

  it should "parse an optional Byte value" in {
    import ParseableOption._
    implicitly[Parseable[Option[Byte]]].parse("1") shouldBe Success(Some(1))
    implicitly[Parseable[Option[Byte]]].parse("") shouldBe Success(None)
  }

  it should "parse an optional Short value" in {
    import ParseableOption._
    implicitly[Parseable[Option[Short]]].parse("1") shouldBe Success(Some(1))
    implicitly[Parseable[Option[Short]]].parse("") shouldBe Success(None)
  }

  it should "parse an optional Float value" in {
    import ParseableOption._
    implicitly[Parseable[Option[Float]]].parse("12.12345") shouldBe Success(Some(12.12345F))
    implicitly[Parseable[Option[Float]]].parse("") shouldBe Success(None)
  }

  it should "parse an optional Double value" in {
    import ParseableOption._
    implicitly[Parseable[Option[Double]]].parse("12.123456789") shouldBe Success(Some(12.123456789))
    implicitly[Parseable[Option[Double]]].parse("") shouldBe Success(None)
  }

  it should "parse an optional Long value" in {
    import ParseableOption._
    implicitly[Parseable[Option[Long]]].parse("9223372036854775807") shouldBe Success(Some(9223372036854775807L))
    implicitly[Parseable[Option[Long]]].parse("") shouldBe Success(None)
  }

  it should "parse an optional LocalDate value" in {
    import ParseableOption._
    implicitly[Parseable[Option[LocalDate]]].parse("2021-05-13") shouldBe Success(Some(LocalDate.parse("2021-05-13")))
    implicitly[Parseable[Option[LocalDate]]].parse("") shouldBe Success(None)
  }

  it should "parse an optional URL value" in {
    import ParseableOption._
    implicitly[Parseable[Option[URL]]].parse("https://google.com") shouldBe Success(Some(new URL("https://google.com")))
    implicitly[Parseable[Option[URL]]].parse("") should matchPattern { case Failure(_) => }
  }

  it should "parse an optional File value" in {
    import ParseableOption._
    implicitly[Parseable[Option[File]]].parse("/CellParsersSpec.scala") shouldBe Success(Some(new File("/CellParsersSpec.scala")))
    implicitly[Parseable[Option[File]]].parse("") shouldBe Success(Some(new File("")))
  }

  it should "parse an optional BigInt value" in {
    import ParseableOption._
    implicitly[Parseable[Option[BigInt]]].parse("2") shouldBe Success(Some(BigInt.apply(2)))
    implicitly[Parseable[Option[BigInt]]].parse("") shouldBe Success(None)
  }

  it should "parse an optional BigDecimal value" in {
    import ParseableOption._
    implicitly[Parseable[BigDecimal]].parse("9.9999999999999999") shouldBe Success(BigDecimal("9.9999999999999999"))
    implicitly[Parseable[Option[BigDecimal]]].parse("") shouldBe Success(None)
  }

  // NOTE: please leave this comment here in order to enable testing of the implicitNotFound message
  //  class NotACaseCLass(val x: Int)
  //
  //  behavior of "non-case-classes"
  //  it should "cause a warning message in the compiler" in {
  //    implicitly[Parseable[NotACaseCLass]].parse(1) shouldBe 1
  //  }
}
