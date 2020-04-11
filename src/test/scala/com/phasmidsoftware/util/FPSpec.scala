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

  ignore should "getURLForResource" in {

  }

  ignore should "sequence" in {

  }

}
