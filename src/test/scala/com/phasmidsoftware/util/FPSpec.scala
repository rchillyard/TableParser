/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.util

import com.phasmidsoftware.util.FP._
import org.scalatest.{flatspec, matchers}

import scala.util.{Failure, Success}

class FPSpec extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  behavior of "FPSpec"

  it should "indexFound" in {
    indexFound("junk", 0) shouldBe Success(0)
    indexFound("junk", -1) should matchPattern { case Failure(TableParserException("Header column junk not found", null)) => }
  }

  it should "getURLForResource" in {
    getURLForResource("testFile.txt", getClass) should matchPattern { case Success(_) => }
    getURLForResource(".txt", getClass) should matchPattern { case Failure(_) => }
  }

  it should "sequence" in {
    val try1 = Success(1)
    val try2 = Success(2)
    val try3 = Failure(TableParserException(""))
    sequence(Seq(try1, try2)) shouldBe Success(Seq(1, 2))
    sequence(Seq(try1, try3)) should matchPattern { case Failure(_) => }
  }

}
