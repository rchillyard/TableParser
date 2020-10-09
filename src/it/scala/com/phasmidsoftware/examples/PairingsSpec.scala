/*
 * Copyright (c) 2020. Phasmid Software
 */

package com.phasmidsoftware.examples

import com.phasmidsoftware.examples.Pairings.{convertToPartnerships, prepareJsonFromPartnerships}
import com.phasmidsoftware.table.Table
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.{Failure, Success}

class PairingsSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Pairings"

  it should "prepareJsonFromPartnerships" in {
    val pty = Table.parseResource[Table[Player]]("partnerships.csv", getClass)
    val zy = pty map convertToPartnerships
    val wy = zy flatMap prepareJsonFromPartnerships
    wy should matchPattern { case Success(_) => }
    import spray.json._
    val partnerships = wy.get.parseJson.convertTo[Partnerships]
    partnerships.partnerships.length shouldBe 19
    wy.get.length shouldBe 488
  }


  it should "not prepareJsonFromPartnerships with duplicates" in {
    val pty = Table.parseResource[Table[Player]]("partnershipswithduplicates.csv", getClass)
    val zy = pty map convertToPartnerships
    zy should matchPattern { case Failure(_) => }
    an[Exception] should be thrownBy zy.get
  }

}
