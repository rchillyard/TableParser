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
    //        val airBNBFile = "/AIRBNB.Listing.csv"
    //    val airBNBFile = "/airbnb.csv"
    val airBNBFile = "/airbnb2.csv"
    val mty: Try[RawTable] = Table.parseResourceRaw(airBNBFile)
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    mty match {
      case Success(HeadedTable(r, h)) =>
        println(s"AirBNB: successfully read ${r.size} rows")
        println(s"AirBNB: successfully read ${h.size} columns")
        r.size shouldBe 967
        r take 10 foreach println
    }
  }

}
