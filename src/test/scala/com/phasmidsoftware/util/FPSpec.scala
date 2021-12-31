/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.util

import com.phasmidsoftware.table.TableSpec
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
    indexFound("junk", -1) should matchPattern { case Failure(FPException("Header column 'junk' not found", None)) => }
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

  behavior of "TryUsing"

  it should "return success" in {
    lazy val i: InputStream = getClass.getResourceAsStream("oneLineResource.txt")
    val zy: Try[String] = TryUsing(Source.fromInputStream(i))(s => Try(s.getLines().toList.head))
    zy should matchPattern { case Success("Hello World!") => }
  }

  it should "return failure(0)" in {
    val wy = TryUsing(Source.fromResource(null))(s => Try(s.getLines().toList.head))
    wy should matchPattern { case Failure(_) => }
    wy.recover {
      case _: NullPointerException => Success(())
      case e => fail(s"wrong exception: $e")
    }
  }

  it should "return failure(1)" in {
    lazy val i: InputStream = getClass.getResourceAsStream(null)
    val wy = TryUsing(Source.fromInputStream(i))(s => Try(s.getLines().toList.head))
    wy should matchPattern { case Failure(_) => }
    wy.recover {
      case _: NullPointerException => Success(())
      case e => fail(s"wrong exception: $e")
    }
  }

  it should "return failure(2)" in {
    lazy val i: InputStream = getClass.getResourceAsStream("emptyResource.txt")
    val wy = TryUsing(Source.fromInputStream(i))(s => Try(s.getLines().toList.head))
    wy should matchPattern { case Failure(_) => }
    wy.recover {
      case _: NoSuchElementException => Success(())
      case e => fail(s"wrong exception: $e")
    }
  }

  behavior of "resource"
  it should "get proper URL for FP" in {
    resource[FPSpec]("oneLineResource.txt").isSuccess shouldBe true
  }
  it should "get fail to find URL in wrong package" in {
    resource[FPSpec]("multiline.csv").isSuccess shouldBe false
  }
  it should "get URL from TableSpec package" in {
    resource[TableSpec]("multiline.csv").isSuccess shouldBe true
  }

  behavior of "resourceForClass"
  it should "get resources in this package" in {
    resourceForClass("testFile.txt", getClass) should matchPattern { case Success(_) => }
    resourceForClass("testFile.txt") should matchPattern { case Success(_) => }
    resourceForClass(".txt", getClass) should matchPattern { case Failure(_) => }
  }

}
