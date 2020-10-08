/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.table

import com.phasmidsoftware.parse.{ParserException, TableParserException}
import com.phasmidsoftware.util.FPException
import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.util.{Success, Try}

class RowSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "Row"

  it should "apply(Int) correctly" in {
    val r: Row = Row(Seq("1", "2", "Junk"), Header.create("A", "B", "C"))
    r(0) shouldBe Success("1")
    r(1) shouldBe Success("2")
    r(2) shouldBe Success("Junk")
  }

  it should "fail apply(Int) when appropriate" in {
    val r: Row = Row(Seq("1", "2", "Junk"), Header.create("A", "B", "C"))
    an[IndexOutOfBoundsException] should be thrownBy r(-1).get
    an[ParserException] should be thrownBy r(3).get
    the[ParserException] thrownBy r(3).get should have message "Row: index out of range: 3 (there are 3 elements)"
  }

  it should "apply(String) correctly" in {
    val f: String => Try[String] = Row(Seq("1", "2", "Junk"), Header.create("A", "B", "C"))
    f("A") shouldBe Success("1")
    f("B") shouldBe Success("2")
    f("c") shouldBe Success("Junk")
  }

  it should "fail apply(String) when appropriate" in {
    val f: String => Try[String] = Row(Seq("1", "2", "Junk"), Header(Seq("A", "B", "c")))
    an[FPException] should be thrownBy f("x").get
    the[FPException] thrownBy f("x").get should have message "Header column x not found"
  }

}
