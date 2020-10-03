/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.{Header, Row}
import org.scalatest.flatspec
import org.scalatest.matchers.should

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

}
