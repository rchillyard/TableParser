/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.table.{Header, Row}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.flatspec
import org.scalatest.matchers.should
import scala.util.{Failure, Success}

// Top-level, so that it can use `derives`: 15 fields, more than the 13 that 1.x's cellParserN supported.
case class Wide(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int, l: Int, m: Int, n: Int, o: String) derives CellParser

class CellParsersSpec extends flatspec.AnyFlatSpec with should.Matchers {

  case class MyNumber(x: Int)

  object MyNumber extends CellParsers {
    implicit val helper: ColumnHelper[MyNumber] = columnHelper()
    implicit val parser: CellParser[MyNumber] = CellParser.derived[MyNumber]
    implicit val optParser: CellParser[Option[MyNumber]] = cellParserOption[MyNumber]
  }

  case class PhoneNumber(name: String, x: Long)

  object PhoneNumber extends CellParsers {
    implicit val helper: ColumnHelper[PhoneNumber] = columnHelper()
    implicit val parser: CellParser[PhoneNumber] = CellParser.derived[PhoneNumber]
    implicit val optParser: CellParser[Option[PhoneNumber]] = cellParserOption[PhoneNumber]
    val phoneNumberParserAlt: CellParser[PhoneNumber] = CellParser.derived[PhoneNumber]
  }

  case class MyDate(day: Int, month: String, year: Int)

  object MyDate extends CellParsers {
    implicit val helper: ColumnHelper[MyDate] = columnHelper()
    implicit val parser: CellParser[MyDate] = CellParser.derived[MyDate]
  }

  case class FourTuple(s: String, x: Int, w: String, y: Int)

  object FourTuple extends CellParsers {
    implicit val helper: ColumnHelper[FourTuple] = columnHelper()
    implicit val parser: CellParser[FourTuple] = CellParser.derived[FourTuple]
  }

  /**
   * Case class representing a daily report for raptors at a specific date.
   * CONSIDER using the `DailyRaptorReport` defined in its own module.
   *
   * @param date    the date of the report as a LocalDate
   * @param weather the weather condition reported on that date
   * @param bw      the count of Broad-winged Hawks observed
   * @param rt      the count of Red-tailed Hawks observed
   */
  case class DailyRaptorReport(date: LocalDate, weather: String, bw: Int, rt: Int)

  object DailyRaptorReport extends CellParsers {

    def parseDate(w: String): LocalDate = LocalDate.parse(w, DateTimeFormat.forPattern("MM/dd/yyyy"))

    implicit val dateParser: CellParser[LocalDate] = cellParser(parseDate) // Overrides the standard date parser
    implicit val helper: ColumnHelper[DailyRaptorReport] = columnHelper()
    implicit val parser: CellParser[DailyRaptorReport] = CellParser.derived[DailyRaptorReport]
  }

  object DailyRaptorReportParserISO extends CellParsers {
    implicit val dailyRaptorReportISOColumnHelper: ColumnHelper[DailyRaptorReport] = columnHelper()
    implicit val dailyRaptorReportParserISO: CellParser[DailyRaptorReport] = CellParser.derived[DailyRaptorReport]
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
    val r = RowValues(Row(Seq("1"), Header.create("x"), 0))
    r.convertTo[MyNumber] shouldBe Success(MyNumber(1))
  }

  it should "convertTo PhoneNumber" in {
    val r = RowValues(Row(Seq("Robin", "6171234567"), Header.create("name", "x"), 0))
    r.convertTo[PhoneNumber] shouldBe Success(PhoneNumber("Robin", 6171234567L))
  }

  it should "convertTo PhoneNumber (alt)" in {
    val r = RowValues(Row(Seq("Robin", "6171234567"), Header.create("name", "x"), 0))
    r.convertTo[PhoneNumber](PhoneNumber.phoneNumberParserAlt) shouldBe Success(PhoneNumber("Robin", 6171234567L))
  }

  it should "convertTo MyDate" in {
    val r = RowValues(Row(Seq("21", "March", "2019"), Header.create("day", "month", "year"), 0))
    r.convertTo[MyDate] shouldBe Success(MyDate(21, "March", 2019))
  }

  it should "convertTo Tuple4" in {
    val r = RowValues(Row(Seq("Thursday", "21", "March", "2019"), Header.create("s", "x", "w", "y"), 0))
    r.convertTo[FourTuple] shouldBe Success(FourTuple("Thursday", 21, "March", 2019))
  }

  private val partlyCloudy = "Partly Cloudy"
  it should "convertTo DailyRaptorReport" in {
    val r = RowValues(Row(Seq("09/16/2018", partlyCloudy, "3308", "5"), Header.create("Date", "Weather", "BW", "RT"), 0))
    // TODO fix deprecation here and 7 lines down.
    val date = new LocalDate(2018, 9, 16)
    r.convertTo[DailyRaptorReport] shouldBe Success(DailyRaptorReport(date, partlyCloudy, 3308, 5))
  }

  it should "convertTo DailyRaptorReport in ISO date parse" in {
    val r = RowValues(Row(Seq("2018-09-16", partlyCloudy, "3308", "5"), Header.create("Date", "Weather", "BW", "RT"), 0))
    import DailyRaptorReportParserISO._
    val date = new LocalDate(2018, 9, 16)
    r.convertTo[DailyRaptorReport] shouldBe Success(DailyRaptorReport(date, partlyCloudy, 3308, 5))
  }

  it should "convertTo Seq[Int]" in {
    val r = RowValues(Row(Seq("21", "03", "2019"), Header.create(), 0))
    import IntSeqParser._
    r.convertTo[Seq[Int]] shouldBe Success(List(21, 3, 2019))
  }

  behavior of "CellParsers"

  it should "parse Int" in {
    implicitly[CellParser[Int]].convertString("1") shouldBe Success(1)
  }

  it should "parse Double" in {
    implicitly[CellParser[Double]].convertString("1") shouldBe Success(1.0)
  }

  it should "parse Long" in {
    implicitly[CellParser[Long]].convertString("99") shouldBe Success(99L)
  }

  it should "parse BigInt" in {
    implicitly[CellParser[BigInt]].convertString("999999999999") shouldBe Success(BigInt("999999999999"))
  }

  it should "parse Option[Int]" in {
    implicitly[CellParser[Option[Int]]].convertString("") should matchPattern { case Success(None) => }
    implicitly[CellParser[Option[Int]]].convertString("1") shouldBe Success(Some(1))
  }

  it should "parse optional MyNumber as None" in {
    import MyNumber._

    implicitly[CellParser[Option[MyNumber]]].convertString("") should matchPattern { case Success(None) => }
  }

  it should "parse optional MyNumber(1)" in {
    val cellParser = implicitly[CellParser[Option[MyNumber]]]
    cellParser.convertString("1") shouldBe Success(Some(MyNumber(1)))
  }

  it should "parse optional PhoneNumber" in {
    val r = RowValues(Row(Seq("Robin", "6171234567"), Header.create("name", "x"), 0))
    r.convertTo[Option[PhoneNumber]] shouldBe Success(Some(PhoneNumber("Robin", 6171234567L)))
  }

  it should "parse optional non-empty String" in {
    val parser = StdCellParsers.cellParserOptionNonEmptyString
    parser.convertString("Hello") shouldBe Success(Some("Hello"))
    parser.convertString("") shouldBe Success(None)
  }

  it should "conditionally parse" in {
    sealed trait IsInt {
      val x: Int
    }
    case class Int1(x: Int) extends IsInt
    case class Int2(x: Int) extends IsInt
    case class MyInt(s: Int, z: IsInt)
    val p = StdCellParsers
    implicit val int1ColumnHelper: ColumnHelper[Int1] = p.columnHelper()
    implicit val int2ColumnHelper: ColumnHelper[Int2] = p.columnHelper()
    implicit val myIntColumnHelper: ColumnHelper[MyInt] = p.columnHelper()
    implicit val int1Parser: CellParser[IsInt] = CellParser.derived[Int1]
    implicit val int2Parser: CellParser[IsInt] = CellParser.derived[Int2]
    implicit val parser: CellParser[MyInt] = p.cellParser2Conditional(MyInt.apply, Map(1 -> int1Parser, 2 -> int2Parser))

    RowValues(Row(Seq("1", "2"), Header.create("s", "z"), 0)).convertTo[MyInt].get.z shouldBe Int1(2)
    RowValues(Row(Seq("2", "2"), Header.create("s", "z"), 1)).convertTo[MyInt].get.z shouldBe Int2(2)
  }

  behavior of "CellParsers.cellParsersN"

  it should "parse with cellParser1" in {
    case class T(a: Int)
    implicit val tParser: CellParser[T] = CellParser.derived[T]
    val parser = implicitly[CellParser[T]]
    parser.toString shouldBe "MultiCellParser for T"
    val header = Header.create("a")
    val row = Row(Seq("1"), header, 0)
    parser.parse(None, row, header) shouldBe Success(T(1))
  }

  it should "parse with cellParser2" in {
    case class T(a: Int, b: Double)
    implicit val tParser: CellParser[T] = CellParser.derived[T]
    val parser = implicitly[CellParser[T]]
    parser.toString shouldBe "MultiCellParser for T"
    val header = Header.create("a", "b")
    val row = Row(Seq("1", "2"), header, 0)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0))
  }

  it should "parse with cellParser3" in {
    case class T(a: Int, b: Double, c: Boolean)
    implicit val tParser: CellParser[T] = CellParser.derived[T]
    val parser = implicitly[CellParser[T]]
    parser.toString shouldBe "MultiCellParser for T"
    val header = Header.create("a", "b", "c")
    val row = Row(Seq("1", "2", "true"), header, 0)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, c = true))
  }

  it should "parse with cellParser4" in {
    case class T(a: Int, b: Double, c: String, d: Boolean)
    implicit val tParser: CellParser[T] = CellParser.derived[T]
    val parser = implicitly[CellParser[T]]
    parser.toString shouldBe "MultiCellParser for T"
    val header = Header.create("a", "b", "c", "d")
    val row = Row(Seq("1", "2", "x", "true"), header, 0)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", d = true))
  }

  it should "parse with cellParser5" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Boolean)
    implicit val tParser: CellParser[T] = CellParser.derived[T]
    val parser = implicitly[CellParser[T]]
    parser.toString shouldBe "MultiCellParser for T"
    val header = Header.create("a", "b", "c", "d", "e")
    val row = Row(Seq("1", "2", "x", "128", "true"), header, 0)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, e = true))
  }

  it should "parse with cellParser6" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Byte, f: Boolean)
    implicit val tParser: CellParser[T] = CellParser.derived[T]
    val parser = implicitly[CellParser[T]]
    parser.toString shouldBe "MultiCellParser for T"
    val header = Header.create("a", "b", "c", "d", "e", "f")
    val row = Row(Seq("1", "2", "x", "128", "7", "true"), header, 0)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, 7, f = true))
  }

  it should "parse with cellParser7" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Byte, f: Float, g: Boolean)
    implicit val tParser: CellParser[T] = CellParser.derived[T]
    val parser = implicitly[CellParser[T]]
    parser.toString shouldBe "MultiCellParser for T"
    val header = Header.create("a", "b", "c", "d", "e", "f", "g")
    val row = Row(Seq("1", "2", "x", "128", "7", "3.14", "true"), header, 0)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, 7, 3.14f, g = true))
  }

  it should "parse with cellParser8" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Byte, f: Float, g: BigInt, h: Boolean)
    implicit val tParser: CellParser[T] = CellParser.derived[T]
    val parser = implicitly[CellParser[T]]
    parser.toString shouldBe "MultiCellParser for T"
    val header = Header.create("a", "b", "c", "d", "e", "f", "g", "h")
    val row = Row(Seq("1", "2", "x", "128", "7", "3.14", "1000000", "true"), header, 0)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, 7, 3.14f, BigInt(1000000), h = true))
  }

  it should "parse with cellParser9" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Byte, f: Float, g: BigInt, h: BigDecimal, i: Boolean)
    implicit val tParser: CellParser[T] = CellParser.derived[T]
    val parser = implicitly[CellParser[T]]
    parser.toString shouldBe "MultiCellParser for T"
    val header = Header.create("a", "b", "c", "d", "e", "f", "g", "h", "i")
    val row = Row(Seq("1", "2", "x", "128", "7", "3.14", "1000000", "3.1415927", "true"), header, 0)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, 7, 3.14f, BigInt(1000000), BigDecimal(3.1415927), i = true))
  }

  it should "parse with cellParser10" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Byte, f: Float, g: BigInt, h: BigDecimal, i: Int, j: Boolean)
    implicit val tParser: CellParser[T] = CellParser.derived[T]
    val parser = implicitly[CellParser[T]]
    parser.toString shouldBe "MultiCellParser for T"
    val header = Header.create("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
    val row = Row(Seq("1", "2", "x", "128", "7", "3.14", "1000000", "3.1415927", "99", "true"), header, 0)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, 7, 3.14f, BigInt(1000000), BigDecimal(3.1415927), 99, j = true))
  }

  it should "parse with cellParser11" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Byte, f: Float, g: BigInt, h: BigDecimal, i: Int, j: Char, k: Boolean)
    implicit val tParser: CellParser[T] = CellParser.derived[T]
    val parser = implicitly[CellParser[T]]
    parser.toString shouldBe "MultiCellParser for T"
    val header = Header.create("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")
    val row = Row(Seq("1", "2", "x", "128", "7", "3.14", "1000000", "3.1415927", "99", "a", "true"), header, 0)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, 7, 3.14f, BigInt(1000000), BigDecimal(3.1415927), 99, 'a', k = true))
  }

  it should "parse with cellParser12" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Byte, f: Float, g: BigInt, h: BigDecimal, i: Int, j: Char, k: Double, l: Boolean)
    implicit val tParser: CellParser[T] = CellParser.derived[T]
    val parser = implicitly[CellParser[T]]
    parser.toString shouldBe "MultiCellParser for T"
    val header = Header.create("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")
    val row = Row(Seq("1", "2", "x", "128", "7", "3.14", "1000000", "3.1415927", "99", "a", "3.1415927", "true"), header, 0)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, 7, 3.14f, BigInt(1000000), BigDecimal(3.1415927), 99, 'a', 3.1415927, l = true))
  }

  it should "parse with cellParser13" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Byte, f: Float, g: BigInt, h: BigDecimal, i: Int, j: Char, k: Double, l: Boolean, m: Int)
    implicit val tParser: CellParser[T] = CellParser.derived[T]
    val parser = implicitly[CellParser[T]]
    parser.toString shouldBe "MultiCellParser for T"
    val header = Header.create("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m")
    val row = Row(Seq("1", "2", "x", "128", "7", "3.14", "1000000", "3.1415927", "99", "a", "3.1415927", "true", "42"), header, 0)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, 7, 3.14f, BigInt(1000000), BigDecimal(3.1415927), 99, 'a', 3.1415927, l = true, 42))
  }

  behavior of "AttributeSet"
  it should "behave" in {
    AttributeSet.parse("{x}").get.xs shouldBe List("x")
    AttributeSet.parse("{x,y}").get.xs shouldBe List("x", "y")
  }

  behavior of "CellParser.derived"

  it should "parse a case class that derives CellParser, with more than 13 fields" in {
    val parser = implicitly[CellParser[Wide]]
    parser.toString shouldBe "MultiCellParser for Wide"
    val header = Header.create("o", "n", "m", "l", "k", "j", "i", "h", "g", "f", "e", "d", "c", "b", "a")
    val row = Row(Seq("fifteen", "14", "13", "12", "11", "10", "9", "8", "7", "6", "5", "4", "3", "2", "1"), header, 0)
    parser.parse(None, row, header) shouldBe Success(Wide(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, "fifteen"))
  }
}
