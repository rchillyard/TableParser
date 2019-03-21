package com.phasmidsoftware.format

import com.phasmidsoftware.tableparser.Row
import org.scalatest.{FlatSpec, Matchers}

class FormatsSpec extends FlatSpec with Matchers {

  case class MyNumber(x: Int)

  object MyNumberFormat extends Formats {

    import Formats._

    implicit val myNumberFormat: CellReader[MyNumber] = cellReader1(MyNumber.apply)
  }

  case class PhoneNumber(name: String, x: Long)

  object PhoneNumberFormat extends Formats {

    import Formats._

    implicit val phoneNumberFormat: CellReader[PhoneNumber] = cellReader2(PhoneNumber.apply)
  }

  behavior of "FormatsSpec"

  it should "convertTo Int" in {
    val x = CellValue("1")
    import Formats._
    x.convertTo[Int] shouldBe 1
  }

  it should "convertTo MyNumber" in {
    val r = Row(Seq("1"), Seq("x"))
    val z = RowValues(r, Seq("x"))
    import MyNumberFormat._
    z.convertTo shouldBe MyNumber(1)
  }

  it should "convertTo PhoneNumber" in {
    val r = Row(Seq("Robin", "6173705720"), Seq("name", "x"))
    val z = RowValues(r, Seq("name", "x"))
    import PhoneNumberFormat._
    z.convertTo[PhoneNumber] shouldBe PhoneNumber("Robin", 6173705720L)
  }

}
