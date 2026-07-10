package com.phasmidsoftware.tableparser.core.render

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CsvGeneratorSpec extends AnyFlatSpec with should.Matchers {

  behavior of "CsvGenerator"

  it should "toColumnName BigInt" in {
    val csvGenerator: CsvGenerator[BigInt] = implicitly[CsvGenerator[BigInt]]
    val header = csvGenerator.toColumnName(None, "id")
    header shouldBe "id"
  }
  it should "toColumnName Option[BigInt]" in {
    val csvGenerator: CsvGenerator[Option[BigInt]] = implicitly[CsvGenerator[Option[BigInt]]]
    val header = csvGenerator.toColumnName(None, "maybeId")
    header shouldBe "maybeId" // CONSIDER should this be converted to "id"?
  }

}
