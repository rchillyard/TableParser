package com.phasmidsoftware.util

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.util.EvaluateIO.{check, checkFailure, matchIO}
import org.scalatest.exceptions.TestFailedException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EvaluateIOSpec extends AnyFlatSpec with should.Matchers {

  behavior of "EvaluateIO"

  it should "apply 1" in {
    EvaluateIO(IO(1)) shouldBe 1
  }

  it should "apply 2" in {
    a[RuntimeException] should be thrownBy EvaluateIO(IO.raiseError(new RuntimeException("failure")))
  }

  it should "matchIO 0" in {
    matchIO(IO(0)) {
      case x@1 => fail(s"wrong value: $x")
      case _ => succeed
    }
  }

  it should "matchIO 0A" in {
    a[TestFailedException] shouldBe thrownBy(matchIO(IO(0)) {
      case x if x <= 0 => fail(s"wrong value: $x")
      case _ => succeed
    })
  }

  // This test does the right thing but we don't want to see a failure
//  it should "matchIO 0B" in {
//    EvaluateIO.matchIO(IO(0)) {
//      case x if x <= 0 => fail(s"wrong value: $x")
//      case _ => succeed
//    }
//  }

  it should "matchIO 1" in {
    matchIO(IO(1)) {
      case 1 => succeed
      case x => fail(s"wrong value: $x")
    }
  }

  it should "matchIO 2" in {
    a[RuntimeException] should be thrownBy matchIO(IO.raiseError(new RuntimeException("failure"))) {
      case _ => fail("wrong value")
    }
  }

  it should "matchIO 3" in {
    a[MatchError] should be thrownBy matchIO(IO(2)) {
      case 1 => succeed
    }
  }

  it should "check ok" in {
    check(IO(1)) {
      case 1 => println("OK")
    }
  }

  it should "check not ok" in {
    a[MatchError] shouldBe thrownBy(check(IO(2)) {
      case 1 => println("OK")
    })
  }

  it should "check not failure not ok" in {
    a[TestFailedException] shouldBe thrownBy(checkFailure(IO(1))(classOf[NoSuchElementException]).unsafeRunSync())
  }

  it should "check not failure ok" in {
    checkFailure(IO.raiseError(new NoSuchElementException))(classOf[NoSuchElementException]).unsafeRunSync()
  }

}
