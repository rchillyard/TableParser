package com.phasmidsoftware.parse

import com.phasmidsoftware.table.{Header, Row}
import org.scalatest.{FlatSpec, Matchers}

//noinspection NotImplementedCode
class CellParserSpec extends FlatSpec with Matchers {

  private val intCellParser = new CellParser[Int] {
    def convertString(w: String): Int = w.toInt

    def read(w: Option[String], row: Row, columns: Header): Int = ???
  }

  private val booleanCellParser = new CellParser[Boolean] {
    def convertString(w: String): Boolean = w.toBoolean

    def read(w: Option[String], row: Row, columns: Header): Boolean = ???
  }

  behavior of "CellParser"

  it should "read Int" in {
    intCellParser.read(CellValue("1")) shouldBe 1
  }

  it should "read Boolean" in {
    booleanCellParser.read(CellValue("true")) shouldBe true
  }

}
