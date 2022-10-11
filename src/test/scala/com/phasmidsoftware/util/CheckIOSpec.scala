package com.phasmidsoftware.util

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.util.CheckIO.{checkFailureIO, checkResultIO}
import org.scalatest.exceptions.TestFailedException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CheckIOSpec extends AnyFlatSpec with should.Matchers {

  behavior of "CheckIO"

  it should "checkResultIO ok" in {
    checkResultIO(IO(1)) {
      case 1 => println("OK")
    }
  }

  it should "checkResultIO not ok" in {
    a[MatchError] shouldBe thrownBy(checkResultIO(IO(2)) {
      case 1 => println("OK")
    })
  }

  it should "check not failure not ok" in {
    a[TestFailedException] shouldBe thrownBy(checkFailureIO(IO(1))(classOf[NoSuchElementException]).unsafeRunSync())
  }

  it should "check not failure ok" in {
    checkFailureIO(IO.raiseError(new NoSuchElementException))(classOf[NoSuchElementException]).unsafeRunSync()
  }

}
