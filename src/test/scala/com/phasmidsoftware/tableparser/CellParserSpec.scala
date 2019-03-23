package com.phasmidsoftware.tableparser

import org.scalatest.{FlatSpec, Matchers}

//noinspection NotImplementedCode
class CellParserSpec extends FlatSpec with Matchers {

  private val intCellParser = new CellParser[Int] {
    def convertString(w: String): Int = w.toInt

    def read(row: Row, columns: Seq[String]): Int = ???
  }

  private val booleanCellParser = new CellParser[Boolean] {
    def convertString(w: String): Boolean = w.toBoolean

    def read(row: Row, columns: Seq[String]): Boolean = ???
  }

  behavior of "CellParser"

  it should "read Int" in {
    intCellParser.read(CellValue("1")) shouldBe 1
  }

  it should "read Boolean" in {
    booleanCellParser.read(CellValue("true")) shouldBe true
  }

}
