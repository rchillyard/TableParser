package com.phasmidsoftware.util

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.util.EvaluateIO.{check, checkFailure}
import org.scalatest.exceptions.TestFailedException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EvaluateIOSpec extends AnyFlatSpec with should.Matchers {

  behavior of "EvaluateIO"

  it should "apply ok" in {
    EvaluateIO(IO(1)) shouldBe 1
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
