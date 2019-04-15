/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.{Header, Row}
import org.scalatest.{FlatSpec, Matchers}

//noinspection NotImplementedCode
class CellParserSpec extends FlatSpec with Matchers {

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

}
