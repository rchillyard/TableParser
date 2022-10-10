package com.phasmidsoftware.util

import cats.effect.IO
import com.phasmidsoftware.util.CheckIO.checkResultIO
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

}
