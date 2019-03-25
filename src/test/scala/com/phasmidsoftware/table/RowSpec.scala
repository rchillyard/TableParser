package com.phasmidsoftware.table

import com.phasmidsoftware.parse.ParserException
import org.scalatest.{FlatSpec, Matchers}

class RowSpec extends FlatSpec with Matchers {

  behavior of "Row"

  it should "apply(Int) correctly" in {
    val r: Row = Row(Seq("1", "2", "Junk"), Seq("A", "B", "C"))
    r(0) shouldBe "1"
    r(1) shouldBe "2"
    r(2) shouldBe "Junk"
  }

  it should "fail apply(Int) when appropriate" in {
    val r: Row = Row(Seq("1", "2", "Junk"), Seq("A", "B", "C"))
    an[IndexOutOfBoundsException] should be thrownBy r(-1)
    an[ParserException] should be thrownBy r(3)
    the[ParserException] thrownBy r(3) should have message "Row: index out of range: 3 (there are 3 elements)"
  }

  it should "apply(String) correctly" in {
    val f: String => String = Row(Seq("1", "2", "Junk"), Seq("A", "B", "C"))
    f("A") shouldBe "1"
    f("B") shouldBe "2"
    f("c") shouldBe "Junk"
  }

  it should "fail apply(String) when appropriate" in {
    val f: String => String = Row(Seq("1", "2", "Junk"), Seq("A", "B", "c"))
    an[ParserException] should be thrownBy f("x")
    an[ParserException] should be thrownBy f("c")
    the[ParserException] thrownBy f("x") should have message "Row: unknown column: x"
  }

}
