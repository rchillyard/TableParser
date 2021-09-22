/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.util

import com.phasmidsoftware.util.FP._
import org.scalatest.flatspec
import org.scalatest.matchers.should

import java.io.InputStream
import scala.io.Source
import scala.util.{Failure, Success, Try}

class FPSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "FP"

  it should "indexFound" in {
    indexFound("junk", 0) shouldBe Success(0)
    indexFound("junk", -1) should matchPattern { case Failure(FPException("Header column junk not found", None)) => }
  }

  it should "getURLForResource" in {
    getURLForResource("testFile.txt", getClass) should matchPattern { case Success(_) => }
    getURLForResource(".txt", getClass) should matchPattern { case Failure(_) => }
  }

  it should "sequence" in {
    val try1 = Success(1)
    val try2 = Success(2)
    val try3 = Failure(FPException(""))
    sequence(Seq(try1, try2)) shouldBe Success(Seq(1, 2))
    val result: Try[Seq[Int]] = sequence(Seq(try1, try3))
    result should matchPattern { case Failure(_) => }
  }

  it should "partition 1" in {
    val try1 = Success(1)
    val try2 = Success(2)
    val (good, bad) = partition(Seq(try1, try2).iterator)
    good.toSeq shouldBe List(try1, try2)
    bad.isEmpty shouldBe true
  }
  it should "partition 2" in {
    val try1 = Success(1)
    val try3 = Failure(FPException(""))
    val (good, bad) = partition(Seq(try1, try3).iterator)
    good.toSeq shouldBe List(try1)
    bad.toSeq shouldBe List(try3)
  }

  behavior of "safeResource"

  it should "return success" in {
    lazy val i: InputStream = getClass.getResourceAsStream("oneLineResource.txt")
    val zy: Try[String] = safeResource(Source.fromInputStream(i))(s => Try(s.getLines().toList.head))
    zy should matchPattern { case Success("Hello World!") => }
  }

  // 2.12
  ignore should "return failure(0)" in {
    val wy = safeResource(Source.fromResource(null))(s => Try(s.getLines().toList.head))
    wy should matchPattern { case Failure(_) => }
    wy.recover {
      case _: NullPointerException => Success(())
      case e => fail(s"wrong exception: $e")
    }
  }

  // 2.12
  ignore should "return failure(1)" in {
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
