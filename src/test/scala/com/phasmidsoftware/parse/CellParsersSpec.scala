/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import java.util.Date

import com.phasmidsoftware.table.{Header, Row}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

class CellParsersSpec extends FlatSpec with Matchers {

  case class MyNumber(x: Int)

  object MyNumberParser extends CellParsers {

    implicit val myNumberColumnHelper: ColumnHelper[MyNumber] = columnHelper()
    implicit val myNumberParser: CellParser[MyNumber] = cellParser1(MyNumber)
  }

  case class PhoneNumber(name: String, x: Long)

  object PhoneNumberParser extends CellParsers {

    implicit val phoneNumberColumnHelper: ColumnHelper[PhoneNumber] = columnHelper()
    implicit val phoneNumberParser: CellParser[PhoneNumber] = cellParser2(PhoneNumber)
  }

  case class MyDate(day: Int, month: String, year: Int)

  object MyDateParser extends CellParsers {

    implicit val myDateColumnHelper: ColumnHelper[MyDate] = columnHelper()
    implicit val myDateParser: CellParser[MyDate] = cellParser3(MyDate)
  }

  case class FourTuple(s: String, x: Int, w: String, y: Int)

  object FourTupleParser extends CellParsers {

    implicit val fourTupleColumnHelper: ColumnHelper[FourTuple] = columnHelper()
    implicit val fourTupleParser: CellParser[FourTuple] = cellParser4(FourTuple)
  }

  case class DailyRaptorReport(date: LocalDate, weather: String, bw: Int, rt: Int)

  object DailyRaptorReportParser extends CellParsers {

    private val raptorReportDateFormatter = DateTimeFormat.forPattern("MM/dd/yyyy")

    def parseDate(w: String): LocalDate = LocalDate.parse(w, raptorReportDateFormatter)

    implicit val dateParser: CellParser[LocalDate] = cellParser(parseDate)
    implicit val dailyRaptorReportColumnHelper: ColumnHelper[DailyRaptorReport] = columnHelper()
    implicit val dailyRaptorReportParser: CellParser[DailyRaptorReport] = cellParser4(DailyRaptorReport)
  }

  object DailyRaptorReportParserISO extends CellParsers {

    implicit val dailyRaptorReportISOColumnHelper: ColumnHelper[DailyRaptorReport] = columnHelper()
    implicit val dailyRaptorReportParserISO: CellParser[DailyRaptorReport] = cellParser4(DailyRaptorReport)
  }

  /**
    * NOTE: only used in testing currently
    */
  object IntSeqParser extends CellParsers {

    implicit val intSeqParser: CellParser[Seq[Int]] = cellParserSeq
  }

  behavior of "Convertibles"

  it should "convertTo Int" in {
    val x = CellValue("1")
    x.convertTo[Int] shouldBe Success(1)
  }

  it should "convertTo Double" in {
    val x = CellValue("1.0")
    x.convertTo[Double] shouldBe Success(1.0)
  }

  it should "convertTo MyNumber" in {
    val r = RowValues(Row(Seq("1"), Header.create("x")))
    import MyNumberParser._
    r.convertTo shouldBe Success(MyNumber(1))
  }

  it should "convertTo PhoneNumber" in {
    val r = RowValues(Row(Seq("Robin", "6171234567"), Header.create("name", "x")))
    import PhoneNumberParser._
    r.convertTo[PhoneNumber] shouldBe Success(PhoneNumber("Robin", 6171234567L))
  }

  it should "convertTo MyDate" in {
    val r = RowValues(Row(Seq("21", "March", "2019"), Header.create("day", "month", "year")))
    import MyDateParser._
    r.convertTo[MyDate] shouldBe Success(MyDate(21, "March", 2019))
  }

  it should "convertTo Tuple4" in {
    val r = RowValues(Row(Seq("Thursday", "21", "March", "2019"), Header.create("s", "x", "w", "y")))
    import FourTupleParser._
    r.convertTo[FourTuple] shouldBe Success(FourTuple("Thursday", 21, "March", 2019))
  }

  it should "convertTo DailyRaptorReport" in {
    val r = RowValues(Row(Seq("09/16/2018", "Partly Cloudy", "3308", "5"), Header.create("Date", "Weather", "BW", "RT")))
    import DailyRaptorReportParser._
    //noinspection ScalaDeprecation
    r.convertTo[DailyRaptorReport] shouldBe Success(DailyRaptorReport(LocalDate.fromDateFields(new Date(118, 8, 16)), "Partly Cloudy", 3308, 5))
  }

  it should "convertTo DailyRaptorReport in ISO date parse" in {
    val r = RowValues(Row(Seq("2018-09-16", "Partly Cloudy", "3308", "5"), Header.create("Date", "Weather", "BW", "RT")))
    import DailyRaptorReportParserISO._
    //noinspection ScalaDeprecation
    r.convertTo[DailyRaptorReport] shouldBe Success(DailyRaptorReport(LocalDate.fromDateFields(new Date(118, 8, 16)), "Partly Cloudy", 3308, 5))
  }

  it should "convertTo Seq[Int]" in {
    val r = RowValues(Row(Seq("21", "03", "2019"), Header.create()))
    import IntSeqParser._
    r.convertTo[Seq[Int]] shouldBe Success(List(21, 3, 2019))
  }

  behavior of "CellParsers"

  it should "parse Int" in {
    implicitly[CellParser[Int]].convertString("1") shouldBe 1
  }

  it should "parse Double" in {
    implicitly[CellParser[Double]].convertString("1") shouldBe 1.0
  }

  it should "parse Long" in {
    implicitly[CellParser[Long]].convertString("99") shouldBe 99L
  }

  it should "parse BigInt" in {
    implicitly[CellParser[BigInt]].convertString("999999999999") shouldBe BigInt("999999999999")
  }

  it should "parse Option[Int]" in {
    implicitly[CellParser[Option[Int]]].convertString("") shouldBe None
    implicitly[CellParser[Option[Int]]].convertString("1") shouldBe Some(1)
  }

  it should "parse optional MyNumber as None" in {
    import MyNumberParser._

    implicit val optionalMyNumberParser: CellParser[Option[MyNumber]] = cellParserOption[MyNumber]
    implicitly[CellParser[Option[MyNumber]]].convertString("") shouldBe None
  }

  it should "parse optional MyNumber(1)" in {
    import MyNumberParser._

    implicit val optionalMyNumberParser: CellParser[Option[MyNumber]] = cellParserOption[MyNumber]
    val cellParser = implicitly[CellParser[Option[MyNumber]]]
    cellParser.convertString("1") shouldBe Some(MyNumber(1))
  }

  it should "parse optional PhoneNumber" in {
    import PhoneNumberParser._
    implicit val optionalPhoneNumberParser: CellParser[Option[PhoneNumber]] = cellParserOption[PhoneNumber]
    val r = RowValues(Row(Seq("Robin", "6171234567"), Header.create("name", "x")))
    r.convertTo[Option[PhoneNumber]] shouldBe Success(Some(PhoneNumber("Robin", 6171234567L)))
  }

}
