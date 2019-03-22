package com.phasmidsoftware.format

import com.phasmidsoftware.tableparser.{CellParser, CellValue, Row, RowValues}
import org.scalatest.{FlatSpec, Matchers}

class FormatsSpec extends FlatSpec with Matchers {

  case class MyNumber(x: Int)

  object MyNumberFormat extends Formats {

    import Formats._

    implicit val myNumberFormat: CellParser[MyNumber] = cellReader1(MyNumber)
  }

  case class PhoneNumber(name: String, x: Long)

  object PhoneNumberFormat extends Formats {

    import Formats._

    implicit val phoneNumberFormat: CellParser[PhoneNumber] = cellReader2(PhoneNumber)
  }

  case class MyDate(day: Int, month: String, year: Int)

  object MyDateFormat extends Formats {

    import Formats._

    implicit val myDateFormat: CellParser[MyDate] = cellReader3(MyDate)
  }

  case class FourTuple(s: String, x: Int, w: String, y: Int)

  object FourTupleFormat extends Formats {

    import Formats._

    implicit val fourTupleFormat: CellParser[FourTuple] = cellReader4(FourTuple)
  }

  object IntSeqFormat extends Formats {

    import Formats._

    implicit val intSeqFormat: CellParser[Seq[Int]] = cellReaderSeq
  }

  behavior of "FormatsSpec"

  it should "convertTo Int" in {
    val x = CellValue("1")
    import Formats._
    x.convertTo[Int] shouldBe 1
  }

  it should "convertTo MyNumber" in {
    val r = RowValues(Row(Seq("1"), Seq("X")))
    import MyNumberFormat._
    r.convertTo shouldBe MyNumber(1)
  }

  it should "convertTo PhoneNumber" in {
    val r = RowValues(Row(Seq("Robin", "6171234567"), Seq("NAME", "X")))
    import PhoneNumberFormat._
    r.convertTo[PhoneNumber] shouldBe PhoneNumber("Robin", 6171234567L)
  }

  it should "convertTo MyDate" in {
    val r = RowValues(Row(Seq("21", "March", "2019"), Seq("DAY", "MONTH", "YEAR")))
    import MyDateFormat._
    r.convertTo[MyDate] shouldBe MyDate(21, "March", 2019)
  }

  it should "convertTo Tuple4" in {
    val r = RowValues(Row(Seq("Thursday", "21", "March", "2019"), Seq("S", "X", "W", "Y")))
    import FourTupleFormat._
    r.convertTo[FourTuple] shouldBe FourTuple("Thursday", 21, "March", 2019)
  }

  it should "convertTo Seq[Int]" in {
    val r = RowValues(Row(Seq("21", "03", "2019"), Nil))
    import IntSeqFormat._
    r.convertTo[Seq[Int]] shouldBe List(21, 3, 2019)
  }

}
