package com.phasmidsoftware.table

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util._

class AnalysisSpec extends AnyFlatSpec with Matchers {

  behavior of "Analysis"

  /**
    * NOTE: it is perfectly proper for there to be a number of parsing problems.
    * These are application-specific and are not indicative of any bugs in the
    * TableParser library itself.
    */
  it should "be correct for airbnb2.csv" in {
    val airBNBFile = "/airbnb2.csv"
    val mty: Try[RawTable] = Table.parseResourceRaw(airBNBFile)
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    mty match {
      case Success(t@HeadedTable(r, _)) =>
        val analysis = Analysis(t)
        analysis.rows shouldBe 254
        analysis.columns shouldBe 87
        analysis.columnMap.size shouldBe 87
        analysis.columnMap("bedrooms") should matchPattern { case Column("Int", _) => }
      //        r take 254 foreach println
    }
  }

}
