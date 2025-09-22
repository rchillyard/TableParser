package com.phasmidsoftware.tableparser.zio.util

import com.phasmidsoftware.tableparser.zio.util.EvaluateZIO.matchZIO
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import zio._

class EvaluateZIOSpec extends AnyFlatSpec with should.Matchers {

  behavior of "matchZIO"

  it should "apply 1" in {
    matchZIO(ZIO.succeed(1)) {
      case 1 => true
      case _ => false
    }
  }

//  it should "apply 2" in {
//    a[RuntimeException] should be thrownBy EvaluateZIO(IO.raiseError(new RuntimeException("failure")))
//  }

  it should "matchZIO 0" in {
    matchZIO(ZIO.succeed(0)) {
      case x@1 => fail(s"wrong value: $x")
      case _ => true
    }
  }

//  it should "matchZIO 0A" in {
//    a[TestFailedException] shouldBe thrownBy(matchZIO(ZIO.succeed(0)) {
//      case x if x <= 0 => org.scalatest.Assertions.fail(s"wrong value: $x")
//      case _ => true
//    })
//  }

  // This test does the right thing but we don't want to see a failure
//  it should "matchZIO 0B" in {
//    EvaluateZIO.matchZIO(IO(0)) {
//      case x if x <= 0 => fail(s"wrong value: $x")
//      case _ => succeed
//    }
//  }

  it should "matchZIO 1" in {
    matchZIO(ZIO.succeed(1)) {
      case 1 => true
      case x => fail(s"wrong value: $x")
    }
  }

//  it should "matchZIO 2" in {
//    a[RuntimeException] should be thrownBy matchZIO(IO.raiseError(new RuntimeException("failure"))) {
//      case _ => fail("wrong value")
//    }
//  }

//  it should "matchZIO 3" in {
//    a[MatchError] should be thrownBy matchZIO(IO(2)) {
//      case 1 => succeed
//    }
//  }

//  it should "check ok" in {
//    check(IO(1)) {
//      case 1 => println("OK")
//    }
//  }

//  it should "check not ok" in {
//    a[MatchError] shouldBe thrownBy(check(IO(2)) {
//      case 1 => println("OK")
//    })
//  }

//  it should "check not failure not ok" in {
//    // TODO eliminate use of unsafe methods
//    a[TestFailedException] shouldBe thrownBy(checkFailure(IO(1))(classOf[NoSuchElementException]).unsafeRunSync())
//  }

//  it should "check not failure ok" in {
//    // TODO eliminate use of unsafe methods
//    checkFailure(IO.raiseError(new NoSuchElementException))(classOf[NoSuchElementException]).unsafeRunSync()
//  }

}
