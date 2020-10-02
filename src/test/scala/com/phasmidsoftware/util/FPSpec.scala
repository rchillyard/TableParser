/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.util

import java.io.InputStream

import com.phasmidsoftware.util.FP._
import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.io.Source
import scala.util.{Failure, Success, Try}

class FPSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "FP"

  it should "indexFound" in {
    indexFound("junk", 0) shouldBe Success(0)
    indexFound("junk", -1) should matchPattern { case Failure(TableParserException("Header column junk not found", null)) => }
  }

  ignore should "getURLforResource" in {

  }

  ignore should "sequence" in {

  }

  behavior of "safeResource"

  it should "return success" in {
    lazy val i: InputStream = getClass.getResourceAsStream("oneLineResource.txt")
    val zy: Try[String] = safeResource(Source.fromInputStream(i))(s => Try(s.getLines().toList.head))
    zy should matchPattern { case Success("Hello World!") => }
  }

  it should "return failure(0)" in {
    val wy = safeResource(Source.fromResource(null))(s => Try(s.getLines().toList.head))
    wy should matchPattern { case Failure(_) => }
    wy.recover {
      case _: NullPointerException => Success(())
      case e => fail(s"wrong exception: $e")
    }
  }

  it should "return failure(1)" in {
    lazy val i: InputStream = getClass.getResourceAsStream(null)
    val wy = safeResource(Source.fromInputStream(i))(s => Try(s.getLines().toList.head))
    wy should matchPattern { case Failure(_) => }
    wy.recover {
      case _: NullPointerException => Success(())
      case e => fail(s"wrong exception: $e")
    }
  }

  it should "return failure(2)" in {
    lazy val i: InputStream = getClass.getResourceAsStream("emptyResource.txt")
    val wy = safeResource(Source.fromInputStream(i))(s => Try(s.getLines().toList.head))
    wy should matchPattern { case Failure(_) => }
    wy.recover {
      case _: NoSuchElementException => Success(())
      case e => fail(s"wrong exception: $e")
    }
  }

}
