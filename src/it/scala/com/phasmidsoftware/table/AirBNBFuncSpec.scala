package com.phasmidsoftware.table

import com.phasmidsoftware.parse.TableParser.sampler
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

import scala.util._

class AirBNBFuncSpec extends AnyFlatSpec with Matchers {

  behavior of "AirBNB table"

  /**
    * This test takes a long time so we generally ignore it.
    */
  it should "be ingested properly" taggedAs Slow in {
    val airBNBFile = "/AIRBNB.Listing.csv"
    val linesInFile = 104999
    val n = 20
    val mty: Try[RawTable] = Table.parseResourceRaw(airBNBFile, sampler(n))
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    mty match {
      case Success(HeadedTable(r, h)) =>
        println(s"AirBNB: successfully read ${r.size} rows")
        println(s"AirBNB: successfully read ${h.size} columns")
        r.size shouldBe linesInFile / n +- 100
        r take 254 foreach println
      case _ =>
    }
  }
}
