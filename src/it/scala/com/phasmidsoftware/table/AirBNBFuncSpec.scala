package com.phasmidsoftware.table

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util._

class AirBNBFuncSpec extends AnyFlatSpec with Matchers {

  behavior of "AirBNB table"

  /**
    * This test takes a long time so we generally ignore it.
    */
  ignore should "be ingested properly" in {
    val airBNBFile = "/AIRBNB.Listing.csv"
    val mty: Try[RawTable] = Table.parseResourceRaw(airBNBFile)
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    mty match {
      case Success(HeadedTable(r, h)) =>
        println(s"AirBNB: successfully read ${r.size} rows")
        println(s"AirBNB: successfully read ${h.size} columns")
        r.size shouldBe 104999
        r take 254 foreach println
    }
  }

}
