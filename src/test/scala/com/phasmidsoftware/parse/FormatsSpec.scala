package com.phasmidsoftware.parse

import java.util.Date

import com.phasmidsoftware.table.{Header, Row}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

class FormatsSpec extends FlatSpec with Matchers {

  case class MyNumber(x: Int)

  object MyNumberFormat extends Formats {

    implicit val myNumberColumnHelper: ColumnHelper[MyNumber] = columnHelper()
    implicit val myNumberFormat: CellParser[MyNumber] = cellReader1(MyNumber)
  }

  case class PhoneNumber(name: String, x: Long)

  object PhoneNumberFormat extends Formats {

    implicit val phoneNumberColumnHelper: ColumnHelper[PhoneNumber] = columnHelper()
    implicit val phoneNumberFormat: CellParser[PhoneNumber] = cellReader2(PhoneNumber)
  }

  case class MyDate(day: Int, month: String, year: Int)

  object MyDateFormat extends Formats {

    implicit val myDateColumnHelper: ColumnHelper[MyDate] = columnHelper()
    implicit val myDateFormat: CellParser[MyDate] = cellReader3(MyDate)
  }

  case class FourTuple(s: String, x: Int, w: String, y: Int)

  object FourTupleFormat extends Formats {

    implicit val fourTupleColumnHelper: ColumnHelper[FourTuple] = columnHelper()
    implicit val fourTupleFormat: CellParser[FourTuple] = cellReader4(FourTuple)
  }

  case class DailyRaptorReport(date: LocalDate, weather: String, bw: Int, rt: Int)

  object DailyRaptorReportFormat extends Formats {

    private val raptorReportDateFormatter = DateTimeFormat.forPattern("MM/dd/yyyy")

    def parseDate(w: String): LocalDate = LocalDate.parse(w, raptorReportDateFormatter)

    implicit val dateFormat: CellParser[LocalDate] = cellReader(parseDate)
    implicit val dailyRaptorReportColumnHelper: ColumnHelper[DailyRaptorReport] = columnHelper()
    implicit val dailyRaptorReportFormat: CellParser[DailyRaptorReport] = cellReader4(DailyRaptorReport)
  }

  object DailyRaptorReportFormatISO extends Formats {

    implicit val dailyRaptorReportISOColumnHelper: ColumnHelper[DailyRaptorReport] = columnHelper()
    implicit val dailyRaptorReportFormatISO: CellParser[DailyRaptorReport] = cellReader4(DailyRaptorReport)
  }

  /**
    * NOTE: only used in testing currently
    */
  object IntSeqFormat extends Formats {

    implicit val intSeqFormat: CellParser[Seq[Int]] = cellReaderSeq
  }

  behavior of "FormatsSpec"

  it should "convertTo Int" in {
    val x = CellValue("1")
    x.convertTo[Int] shouldBe Success(1)
  }

  it should "convertTo MyNumber" in {
    val r = RowValues(Row(Seq("1"), Header.create("x")))
    import MyNumberFormat._
    r.convertTo shouldBe Success(MyNumber(1))
  }

  it should "convertTo PhoneNumber" in {
    val r = RowValues(Row(Seq("Robin", "6171234567"), Header.create("name", "x")))
    import PhoneNumberFormat._
    r.convertTo[PhoneNumber] shouldBe Success(PhoneNumber("Robin", 6171234567L))
  }

  it should "convertTo MyDate" in {
    val r = RowValues(Row(Seq("21", "March", "2019"), Header.create("day", "month", "year")))
    import MyDateFormat._
    r.convertTo[MyDate] shouldBe Success(MyDate(21, "March", 2019))
  }

  it should "convertTo Tuple4" in {
    val r = RowValues(Row(Seq("Thursday", "21", "March", "2019"), Header.create("s", "x", "w", "y")))
    import FourTupleFormat._
    r.convertTo[FourTuple] shouldBe Success(FourTuple("Thursday", 21, "March", 2019))
  }

  it should "convertTo DailyRaptorReport" in {
    val r = RowValues(Row(Seq("09/16/2018", "Partly Cloudy", "3308", "5"), Header.create("Date", "Weather", "BW", "RT")))
    import DailyRaptorReportFormat._
    //noinspection ScalaDeprecation
    r.convertTo[DailyRaptorReport] shouldBe Success(DailyRaptorReport(LocalDate.fromDateFields(new Date(118, 8, 16)), "Partly Cloudy", 3308, 5))
  }

  it should "convertTo DailyRaptorReport in ISO date parse" in {
    val r = RowValues(Row(Seq("2018-09-16", "Partly Cloudy", "3308", "5"), Header.create("Date", "Weather", "BW", "RT")))
    import DailyRaptorReportFormatISO._
    //noinspection ScalaDeprecation
    r.convertTo[DailyRaptorReport] shouldBe Success(DailyRaptorReport(LocalDate.fromDateFields(new Date(118, 8, 16)), "Partly Cloudy", 3308, 5))
  }

  it should "convertTo Seq[Int]" in {
    val r = RowValues(Row(Seq("21", "03", "2019"), Header.create()))
    import IntSeqFormat._
    r.convertTo[Seq[Int]] shouldBe Success(List(21, 3, 2019))
  }

}
