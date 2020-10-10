/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.{Header, Row}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.util.{Failure, Success}

class CellParsersSpec extends flatspec.AnyFlatSpec with should.Matchers {

  case class MyNumber(x: Int)

  case class PhoneNumber(name: String, x: Long)

  case class MyDate(day: Int, month: String, year: Int)

  case class FourTuple(s: String, x: Int, w: String, y: Int)

  case class DailyRaptorReport(date: LocalDate, weather: String, bw: Int, rt: Int)

  object MyNumberParser extends CellParsers {

    implicit val myNumberColumnHelper: ColumnHelper[MyNumber] = columnHelper()
    implicit val myNumberParser: CellParser[MyNumber] = cellParser1(MyNumber)
  }

  object PhoneNumberParser extends CellParsers {

    implicit val phoneNumberColumnHelper: ColumnHelper[PhoneNumber] = columnHelper()
    implicit val phoneNumberParser: CellParser[PhoneNumber] = cellParser2(PhoneNumber)
    val phoneNumberParserAlt: CellParser[PhoneNumber] = cellParser2(PhoneNumber, Seq("name", "x"))
  }

  object MyDateParser extends CellParsers {

    implicit val myDateColumnHelper: ColumnHelper[MyDate] = columnHelper()
    implicit val myDateParser: CellParser[MyDate] = cellParser3(MyDate)
  }

  object FourTupleParser extends CellParsers {

    implicit val fourTupleColumnHelper: ColumnHelper[FourTuple] = columnHelper()
    implicit val fourTupleParser: CellParser[FourTuple] = cellParser4(FourTuple)
  }

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

  it should "convertTo PhoneNumber (alt)" in {
    val r = RowValues(Row(Seq("Robin", "6171234567"), Header.create("name", "x")))
    r.convertTo[PhoneNumber](PhoneNumberParser.phoneNumberParserAlt) shouldBe Success(PhoneNumber("Robin", 6171234567L))
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

  private val partlyCloudy = "Partly Cloudy"
  it should "convertTo DailyRaptorReport" in {
    val r = RowValues(Row(Seq("09/16/2018", partlyCloudy, "3308", "5"), Header.create("Date", "Weather", "BW", "RT")))
    import DailyRaptorReportParser._
    // TODO fix deprecation here and 7 lines down.
    val date = new LocalDate(2018, 9, 16)
    r.convertTo[DailyRaptorReport] shouldBe Success(DailyRaptorReport(date, partlyCloudy, 3308, 5))
  }

  it should "convertTo DailyRaptorReport in ISO date parse" in {
    val r = RowValues(Row(Seq("2018-09-16", partlyCloudy, "3308", "5"), Header.create("Date", "Weather", "BW", "RT")))
    import DailyRaptorReportParserISO._
    val date = new LocalDate(2018, 9, 16)
    r.convertTo[DailyRaptorReport] shouldBe Success(DailyRaptorReport(date, partlyCloudy, 3308, 5))
  }

  it should "convertTo Seq[Int]" in {
    val r = RowValues(Row(Seq("21", "03", "2019"), Header.create()))
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
    import MyNumberParser._

    implicit val optionalMyNumberParser: CellParser[Option[MyNumber]] = cellParserOption[MyNumber]
    implicitly[CellParser[Option[MyNumber]]].convertString("") should matchPattern { case Success(None) => }
  }

  it should "parse optional MyNumber(1)" in {
    import MyNumberParser._

    implicit val optionalMyNumberParser: CellParser[Option[MyNumber]] = cellParserOption[MyNumber]
    val cellParser = implicitly[CellParser[Option[MyNumber]]]
    cellParser.convertString("1") shouldBe Success(Some(MyNumber(1)))
  }

  it should "parse optional PhoneNumber" in {
    import PhoneNumberParser._
    implicit val optionalPhoneNumberParser: CellParser[Option[PhoneNumber]] = cellParserOption[PhoneNumber]
    val r = RowValues(Row(Seq("Robin", "6171234567"), Header.create("name", "x")))
    r.convertTo[Option[PhoneNumber]] shouldBe Success(Some(PhoneNumber("Robin", 6171234567L)))
  }

  it should "parse optional non-empty String" in {
    val parser = new CellParsers {}.cellParserOptionNonEmptyString
    parser.convertString("Hello") shouldBe Success(Some("Hello"))
    parser.convertString("") shouldBe Success(None)
  }

  it should "conditionally parse" in {
    trait IsInt {
      val x: Int
    }
    case class Int1(x: Int) extends IsInt
    case class Int2(x: Int) extends IsInt
    case class MyInt(s: Int, z: IsInt)
    val cellParsers = new CellParsers {}
    implicit val int1ColumnHelper: ColumnHelper[Int1] = cellParsers.columnHelper()
    implicit val int2ColumnHelper: ColumnHelper[Int2] = cellParsers.columnHelper()
    implicit val myIntColumnHelper: ColumnHelper[MyInt] = cellParsers.columnHelper()
    implicit val int1Parser: CellParser[IsInt] = cellParsers.cellParser1(Int1)
    implicit val int2Parser: CellParser[IsInt] = cellParsers.cellParser1(Int2)
    implicit val parser: CellParser[MyInt] = cellParsers.cellParser2Conditional(MyInt.apply, Map(1 -> int1Parser, 2 -> int2Parser))

    RowValues(Row(Seq("1", "2"), Header.create("s", "z"))).convertTo[MyInt].get.z shouldBe Int1(2)
    RowValues(Row(Seq("2", "2"), Header.create("s", "z"))).convertTo[MyInt].get.z shouldBe Int2(2)
  }

  behavior of "CellParsers.cellParsersN"

  it should "parse with cellParser1" in {
    case class T(a: Int)
    val cellParsers = new CellParsers {}
    implicit val columnHelper: ColumnHelper[T] = cellParsers.columnHelper()
    val parser: CellParser[T] = cellParsers.cellParser1(T)
    parser.toString shouldBe s"MultiCellParser: cellParser1 for ${classOf[T].getName}"
    val header = Header.create("a")
    val row = Row(Seq("1"), header)
    parser.parse(None, row, header) shouldBe Success(T(1))
    val badParser: CellParser[T] = cellParsers.cellParser1(T, Seq("a", "b"))
    badParser.parse(None, row, header) should matchPattern { case Failure(_) => }
  }

  it should "parse with cellParser2" in {
    case class T(a: Int, b: Double)
    val cellParsers = new CellParsers {}
    implicit val columnHelper: ColumnHelper[T] = cellParsers.columnHelper()
    val parser: CellParser[T] = cellParsers.cellParser2(T)
    parser.toString shouldBe s"MultiCellParser: cellParser2 for ${classOf[T].getName}"
    val header = Header.create("a", "b")
    val row = Row(Seq("1", "2"), header)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0))
    val badParser: CellParser[T] = cellParsers.cellParser2(T, Seq("a"))
    badParser.parse(None, row, header) should matchPattern { case Failure(_) => }
  }

  it should "parse with cellParser3" in {
    case class T(a: Int, b: Double, c: Boolean)
    val cellParsers = new CellParsers {}
    implicit val columnHelper: ColumnHelper[T] = cellParsers.columnHelper()
    val parser: CellParser[T] = cellParsers.cellParser3(T)
    parser.toString shouldBe s"MultiCellParser: cellParser3 for ${classOf[T].getName}"
    val header = Header.create("a", "b", "c")
    val row = Row(Seq("1", "2", "true"), header)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, c = true))
    val badParser: CellParser[T] = cellParsers.cellParser3(T, Seq("a", "b"))
    badParser.parse(None, row, header) should matchPattern { case Failure(_) => }
  }

  it should "parse with cellParser4" in {
    case class T(a: Int, b: Double, c: String, d: Boolean)
    val cellParsers = new CellParsers {}
    implicit val columnHelper: ColumnHelper[T] = cellParsers.columnHelper()
    val parser: CellParser[T] = cellParsers.cellParser4(T)
    parser.toString shouldBe s"MultiCellParser: cellParser4 for ${classOf[T].getName}"
    val header = Header.create("a", "b", "c", "d")
    val row = Row(Seq("1", "2", "x", "true"), header)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", d = true))
    val badParser: CellParser[T] = cellParsers.cellParser4(T, Seq("a", "b", "c"))
    badParser.parse(None, row, header) should matchPattern { case Failure(_) => }
  }

  it should "parse with cellParser5" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Boolean)
    val cellParsers = new CellParsers {}
    implicit val columnHelper: ColumnHelper[T] = cellParsers.columnHelper()
    val parser: CellParser[T] = cellParsers.cellParser5(T)
    parser.toString shouldBe s"MultiCellParser: cellParser5 for ${classOf[T].getName}"
    val header = Header.create("a", "b", "c", "d", "e")
    val row = Row(Seq("1", "2", "x", "128", "true"), header)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, e = true))
    val badParser: CellParser[T] = cellParsers.cellParser5(T, Seq("a", "b", "c"))
    badParser.parse(None, row, header) should matchPattern { case Failure(_) => }
  }

  it should "parse with cellParser6" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Byte, f: Boolean)
    val cellParsers = new CellParsers {}
    implicit val columnHelper: ColumnHelper[T] = cellParsers.columnHelper()
    val parser: CellParser[T] = cellParsers.cellParser6(T)
    parser.toString shouldBe s"MultiCellParser: cellParser6 for ${classOf[T].getName}"
    val header = Header.create("a", "b", "c", "d", "e", "f")
    val row = Row(Seq("1", "2", "x", "128", "7", "true"), header)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, 7, f = true))
    val badParser: CellParser[T] = cellParsers.cellParser6(T, Seq("a", "b", "c"))
    badParser.parse(None, row, header) should matchPattern { case Failure(_) => }
  }

  it should "parse with cellParser7" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Byte, f: Float, g: Boolean)
    val cellParsers = new CellParsers {}
    implicit val columnHelper: ColumnHelper[T] = cellParsers.columnHelper()
    val parser: CellParser[T] = cellParsers.cellParser7(T)
    parser.toString shouldBe s"MultiCellParser: cellParser7 for ${classOf[T].getName}"
    val header = Header.create("a", "b", "c", "d", "e", "f", "g")
    val row = Row(Seq("1", "2", "x", "128", "7", "3.14", "true"), header)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, 7, 3.14f, g = true))
    val badParser: CellParser[T] = cellParsers.cellParser7(T, Seq("a", "b", "c"))
    badParser.parse(None, row, header) should matchPattern { case Failure(_) => }
  }

  it should "parse with cellParser8" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Byte, f: Float, g: BigInt, h: Boolean)
    val cellParsers = new CellParsers {}
    implicit val columnHelper: ColumnHelper[T] = cellParsers.columnHelper()
    val parser: CellParser[T] = cellParsers.cellParser8(T)
    parser.toString shouldBe s"MultiCellParser: cellParser8 for ${classOf[T].getName}"
    val header = Header.create("a", "b", "c", "d", "e", "f", "g", "h")
    val row = Row(Seq("1", "2", "x", "128", "7", "3.14", "1000000", "true"), header)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, 7, 3.14f, BigInt(1000000), h = true))
    val badParser: CellParser[T] = cellParsers.cellParser8(T, Seq("a", "b", "c"))
    badParser.parse(None, row, header) should matchPattern { case Failure(_) => }
  }

  it should "parse with cellParser9" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Byte, f: Float, g: BigInt, h: BigDecimal, i: Boolean)
    val cellParsers = new CellParsers {}
    implicit val columnHelper: ColumnHelper[T] = cellParsers.columnHelper()
    val parser: CellParser[T] = cellParsers.cellParser9(T)
    parser.toString shouldBe s"MultiCellParser: cellParser9 for ${classOf[T].getName}"
    val header = Header.create("a", "b", "c", "d", "e", "f", "g", "h", "i")
    val row = Row(Seq("1", "2", "x", "128", "7", "3.14", "1000000", "3.1415927", "true"), header)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, 7, 3.14f, BigInt(1000000), BigDecimal(3.1415927), i = true))
    val badParser: CellParser[T] = cellParsers.cellParser9(T, Seq("a", "b", "c"))
    badParser.parse(None, row, header) should matchPattern { case Failure(_) => }
  }

  it should "parse with cellParser10" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Byte, f: Float, g: BigInt, h: BigDecimal, i: Int, j: Boolean)
    val cellParsers = new CellParsers {}
    implicit val columnHelper: ColumnHelper[T] = cellParsers.columnHelper()
    val parser: CellParser[T] = cellParsers.cellParser10(T)
    parser.toString shouldBe s"MultiCellParser: cellParser10 for ${classOf[T].getName}"
    val header = Header.create("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
    val row = Row(Seq("1", "2", "x", "128", "7", "3.14", "1000000", "3.1415927", "99", "true"), header)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, 7, 3.14f, BigInt(1000000), BigDecimal(3.1415927), 99, j = true))
    val badParser: CellParser[T] = cellParsers.cellParser10(T, Seq("a", "b", "c"))
    badParser.parse(None, row, header) should matchPattern { case Failure(_) => }
  }

  it should "parse with cellParser11" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Byte, f: Float, g: BigInt, h: BigDecimal, i: Int, j: Char, k: Boolean)
    val cellParsers = new CellParsers {}
    implicit val columnHelper: ColumnHelper[T] = cellParsers.columnHelper()
    val parser: CellParser[T] = cellParsers.cellParser11(T)
    parser.toString shouldBe s"MultiCellParser: cellParser11 for ${classOf[T].getName}"
    val header = Header.create("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k")
    val row = Row(Seq("1", "2", "x", "128", "7", "3.14", "1000000", "3.1415927", "99", "a", "true"), header)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, 7, 3.14f, BigInt(1000000), BigDecimal(3.1415927), 99, 'a', k = true))
    val badParser: CellParser[T] = cellParsers.cellParser11(T, Seq("a", "b", "c"))
    badParser.parse(None, row, header) should matchPattern { case Failure(_) => }
  }

  it should "parse with cellParser12" in {
    case class T(a: Int, b: Double, c: String, d: Short, e: Byte, f: Float, g: BigInt, h: BigDecimal, i: Int, j: Char, k: Double, l: Boolean)
    val cellParsers = new CellParsers {}
    implicit val columnHelper: ColumnHelper[T] = cellParsers.columnHelper()
    val parser: CellParser[T] = cellParsers.cellParser12(T)
    parser.toString shouldBe s"MultiCellParser: cellParser12 for ${classOf[T].getName}"
    val header = Header.create("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l")
    val row = Row(Seq("1", "2", "x", "128", "7", "3.14", "1000000", "3.1415927", "99", "a", "3.1415927", "true"), header)
    parser.parse(None, row, header) shouldBe Success(T(1, 2.0, "x", 128, 7, 3.14f, BigInt(1000000), BigDecimal(3.1415927), 99, 'a', 3.1415927, l = true))
    val badParser: CellParser[T] = cellParsers.cellParser12(T, Seq("a", "b", "c"))
    badParser.parse(None, row, header) should matchPattern { case Failure(_) => }
  }

  behavior of "AttributeSet"
  it should "behave" in {
    AttributeSet.parse("{x}").get.xs shouldBe List("x")
    AttributeSet.parse("{x,y}").get.xs shouldBe List("x", "y")
  }
}
