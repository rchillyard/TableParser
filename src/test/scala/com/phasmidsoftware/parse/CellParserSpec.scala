/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.{Header, Row}
import org.joda.time.LocalDate
import org.scalatest.flatspec
import org.scalatest.matchers.should

import java.io.File
import java.net.URL
import scala.util.{Failure, Success, Try}

//noinspection NotImplementedCode
class CellParserSpec extends flatspec.AnyFlatSpec with should.Matchers {

  private val intCellParser: CellParser[Int] = new CellParser[Int] {
    def convertString(w: String): Try[Int] = Try(w.toInt)

    def parse(w: Option[String], row: Row, columns: Header): Try[Int] = Failure(ParserException(s"intCellParser does not implement parse method"))

  }

  private val booleanCellParser: CellParser[Boolean] = new CellParser[Boolean] {
    def convertString(w: String): Try[Boolean] = Try(w.toBoolean)

    def parse(w: Option[String], row: Row, columns: Header): Try[Boolean] = Failure(ParserException(s"booleanCellParser does not implement parse method"))

  }

  private val stringCellParser: CellParser[String] = new CellParser[String] {
    def convertString(w: String): Success[String] = Success(w)

    def parse(w: Option[String], row: Row, columns: Header): Try[String] = Failure(ParserException(s"booleanCellParser does not implement parse method"))

  }

  behavior of "CellParser"

  it should "parse Int" in {
    intCellParser.parse(CellValue("1")) shouldBe Success(1)
  }

  it should "parse Boolean" in {
    booleanCellParser.parse(CellValue("true")) shouldBe Success(true)
  }

  it should "parse String" in {
    stringCellParser.parse(CellValue("Hello")) shouldBe Success("Hello")
    stringCellParser.parse(CellValue(""""Hello"""")) shouldBe Success(""""Hello"""")
    // CONSIDER re-instate this test
    //    stringCellParser.parse(CellValue(""""Hello with internal "" Goodbye"""")) shouldBe """"Hello with internal " Goodbye""""
  }

  behavior of "Implicit parsers"

  it should "parse String" in {
    val p = implicitly[CellParser[String]]
    p.convertString("") shouldBe Success("")
    p.convertString("Hello") shouldBe Success("Hello")
  }

  it should "parse Int" in {
    val p = implicitly[CellParser[Int]]
    p.convertString("99") shouldBe Success(99)
    p.convertString("") should matchPattern { case Failure(_) => }
  }

  it should "parse Boolean" in {
    val p = implicitly[CellParser[Boolean]]
    p.convertString("true") shouldBe Success(true)
    p.convertString("") should matchPattern { case Failure(_) => }
  }

  it should "parse Double" in {
    val p = implicitly[CellParser[Double]]
    p.convertString("3.1415927") shouldBe Success(3.1415927)
    p.convertString("") should matchPattern { case Failure(_) => }
  }

  it should "parse File" in {
    val p = implicitly[CellParser[File]]
    p.convertString("/CellParsersSpec.scala") shouldBe Success(new File("/CellParsersSpec.scala"))
    p.convertString("") shouldBe Success(new File(""))
  }

  it should "parse option Double" in {
    val p = implicitly[CellParser[Option[Double]]]
    p.convertString("3.1415927") shouldBe Success(Some(3.1415927))
    p.convertString("") shouldBe Success(None)
    p.convertString("test") should matchPattern { case Failure(_) => }
  }

  it should "parse option Integer" in {
    val p = implicitly[CellParser[Option[Int]]]
    p.convertString("99") shouldBe Success(Some(99))
    p.convertString("") shouldBe Success(None)
    p.convertString("test") should matchPattern { case Failure(_) => }
  }

  it should "parse option Boolean" in {
    val p = implicitly[CellParser[Option[Boolean]]]
    p.convertString("true") shouldBe Success(Some(true))
    p.convertString("") shouldBe Success(None)
    p.convertString("test") should matchPattern { case Failure(_) => }
  }

  it should "parse option Long" in {
    val p = implicitly[CellParser[Option[Long]]]
    p.convertString("9223372036854775807") shouldBe Success(Some(9223372036854775807L))
    p.convertString("") shouldBe Success(None)
    p.convertString("test") should matchPattern { case Failure(_) => }
  }

  it should "parse option LocalDate" in {
    val p = implicitly[CellParser[Option[LocalDate]]]

    p.convertString("2021-05-13") shouldBe Success(Some(LocalDate.parse("2021-05-13")))
    p.convertString("") shouldBe Success(None)
    p.convertString("test") should matchPattern { case Failure(_) => }
  }

  it should "parse option URL" in {
    val p = implicitly[CellParser[Option[URL]]]

    p.convertString("http://google.com") shouldBe Success(Some(new URL("http://google.com")))
    p.convertString("") should matchPattern { case Failure(_) => }
  }

  it should "parse option File" in {
    val p = implicitly[CellParser[Option[File]]]
    p.convertString("/CellParsersSpec.scala") shouldBe Success(Some(new File("/CellParsersSpec.scala")))
    p.convertString("") shouldBe Success(Some(new File("")))
  }
}
