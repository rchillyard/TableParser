package com.phasmidsoftware.table

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util._

class AirBNBFuncSpec extends AnyFlatSpec with Matchers {

  behavior of "AirBNB table"

  /**
    * NOTE: it is perfectly proper for there to be a number of parsing problems.
    * These are application-specific and are not indicative of any bugs in the
    * TableParser library itself.
    */
  it should "be ingested properly" in {
    //    val airBNBFile = "/AIRBNB.Listing.csv"
    val airBNBFile = "/airbnb.csv"
    val mty: Try[RawTable] = Table.parseResourceRaw(airBNBFile, classOf[AirBNBFuncSpec])
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    for (mt <- mty) {
      println(s"AirBNB: successfully read ${mt.size} rows")
      mt.size shouldBe 2
      mt take 10 foreach println
    }
  }

}
