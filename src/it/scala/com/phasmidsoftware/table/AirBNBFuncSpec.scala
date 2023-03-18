package com.phasmidsoftware.table

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AirBNBFuncSpec extends AnyFlatSpec with Matchers {

  behavior of "AirBNB table"

  /**
   * This test must be ignored when pushing to github.
   * This is because:
   * (1) it takes a long time so we generally ignore it;
   * (2) but, more importantly, we do not store the source file in git!
   */
  //  it should "be ingested properly" taggedAs Slow in {
  //    val airBNBFile = "/airbnb2.csv"
  //    //    val linesInFile = 104999
  //    val linesInFile = 994
  //    val n = 20
  //    implicit val p: RawTableParser = RawTableParser().setMultiline(true) //.setPredicate(sampler(n))
  //    val rty: Try[RawTable] = Table.parseResource(airBNBFile)
  ////    val rty: Try[RawTable] = Table.parseResourceRaw(airBNBFile, sampler(n))
  //    rty should matchPattern { case Success(HeadedTable(_, _)) => }
  //    rty match {
  //      case Success(HeadedTable(r, h)) =>
  //        println(s"AirBNB: successfully read ${r.size} rows")
  //        println(s"AirBNB: successfully read ${h.size} columns")
  //        r.size shouldBe linesInFile / 4 +- 200
  //        // Print the first 100 to the console
  //        r take 100 foreach println
  //      case _ =>
  //    }
  //  }
}
