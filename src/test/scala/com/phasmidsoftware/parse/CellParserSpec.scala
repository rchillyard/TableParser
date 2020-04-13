/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.{Header, Row}
import org.scalatest.{flatspec, matchers}

//noinspection NotImplementedCode
class CellParserSpec extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  private val intCellParser: CellParser[Int] = new CellParser[Int] {
    def convertString(w: String): Int = w.toInt

    def parse(w: Option[String], row: Row, columns: Header): Int = throw ParserException(s"intCellParser does not implement parse method")

  }

  private val booleanCellParser: CellParser[Boolean] = new CellParser[Boolean] {
    def convertString(w: String): Boolean = w.toBoolean

    def parse(w: Option[String], row: Row, columns: Header): Boolean = throw ParserException(s"booleanCellParser does not implement parse method")

  }

  private val stringCellParser: CellParser[String] = new CellParser[String] {
    def convertString(w: String): String = w

    def parse(w: Option[String], row: Row, columns: Header): String = throw ParserException(s"booleanCellParser does not implement parse method")

  }

  behavior of "CellParser"

  it should "parse Int" in {
    intCellParser.parse(CellValue("1")) shouldBe 1
  }

  it should "parse Boolean" in {
    booleanCellParser.parse(CellValue("true")) shouldBe true
  }

  it should "parse String" in {
    stringCellParser.parse(CellValue("Hello")) shouldBe "Hello"
    stringCellParser.parse(CellValue(""""Hello"""")) shouldBe """"Hello""""
    // CONSIDER re-instate this test
    //    stringCellParser.parse(CellValue(""""Hello with internal "" Goodbye"""")) shouldBe """"Hello with internal " Goodbye""""
  }

  behavior of "Implicit parsers"

  it should "parse String" in {
    val p = implicitly[CellParser[String]]
    p.convertString("") shouldBe ""
    p.convertString("Hello") shouldBe "Hello"
  }

  it should "parse Int" in {
    val p = implicitly[CellParser[Int]]
    p.convertString("99") shouldBe 99
    a[ParseableException] should be thrownBy p.convertString("")
  }

  it should "parse Boolean" in {
    val p = implicitly[CellParser[Boolean]]
    p.convertString("true") shouldBe true
    a[ParseableException] should be thrownBy p.convertString("")
  }

  it should "parse Double" in {
    val p = implicitly[CellParser[Double]]
    p.convertString("3.1415927") shouldBe 3.1415927
    a[ParseableException] should be thrownBy p.convertString("")
  }

}
