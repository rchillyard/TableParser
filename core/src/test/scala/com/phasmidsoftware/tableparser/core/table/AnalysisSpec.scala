package com.phasmidsoftware.tableparser.core.table

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class AnalysisSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Analysis"

  private val columnMap = Map("header1" -> Column("String", false, None), "header2" -> Column("String", false, None))

  it should "apply 1" in {
    val header = Header.create("header1", "header2")
    val row1: RawRow = RawRow(List("value1", "value2"), header)
    val row2 = RawRow(List("value3", "value4"), header)
    val rawTable: RawTable = Table[RawRow](Seq(row1, row2), Some(header))
    val analysis = Analysis(rawTable)
    analysis.rows shouldBe 2
    analysis.columns shouldBe 2
    analysis.columnMap shouldBe columnMap
  }

  it should "apply 2" in {
    val analysis = Analysis(2, 2, columnMap)
    analysis.rows shouldBe 2
    analysis.columns shouldBe 2
  }

}
