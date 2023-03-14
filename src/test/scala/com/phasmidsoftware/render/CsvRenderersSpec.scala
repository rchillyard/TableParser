package com.phasmidsoftware.render

import cats.effect.IO
import com.phasmidsoftware.parse._
import com.phasmidsoftware.table._
import com.phasmidsoftware.util.EvaluateIO
import com.phasmidsoftware.util.EvaluateIO.matchIO
import java.net.URL
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}


//noinspection SpellCheckingInspection
class CsvRenderersSpec extends AnyFlatSpec with should.Matchers {

  case class IntPair(a: Int, b: Int) {
    def map(f: Int => Int): IntPair = IntPair(f(a), f(b))
  }

  object IntPair {

    object IntPairParser extends JavaTokenParsers {
      lazy val pair: Parser[(Int, Int)] = wholeNumber ~ wholeNumber ^^ { case x ~ y => (x.toInt, y.toInt) }
    }

    trait IntPairRowParser extends StringParser[IntPair] {
      def parse(indexedString: (String, Int))(header: Header): Try[IntPair] = IntPairParser.parseAll(IntPairParser.pair, indexedString._1) match {
        case IntPairParser.Success((x, y), _) => Success(IntPair(x, y))
        case _ => Failure(TableException(s"unable to parse ${indexedString._1}"))
      }

      //noinspection NotImplementedCode
      def parseHeader(w: Seq[String]): IO[Header] = ???
    }

    implicit object IntPairRowParser extends IntPairRowParser

    trait IntPairTableParser extends StringTableParser[Table[IntPair]] {
      type Row = IntPair

      val maybeFixedHeader: Option[Header] = Some(Header.create("a", "b"))

      val headerRowsToRead: Int = 0

      protected def builder(rows: Iterable[IntPair], header: Header): Table[IntPair] = HeadedTable(rows, Header[IntPair]())

      val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]
    }

    implicit object IntPairTableParser extends IntPairTableParser

  }

  behavior of "CsvGenerators"

  it should "generate header for an Int" in {
    import CsvGenerators._
    implicitly[CsvGenerator[Int]].toColumnName(None, "x") shouldBe "x"
    implicitly[CsvGenerator[Int]].toColumnName(Some("x"), "y") shouldBe "x.y"
  }

  it should "generate header for a Long, URL, Double, Boolean" in {
    import CsvGenerators._
    implicitly[CsvGenerator[Long]].toColumnName(None, "x") shouldBe "x"
    implicitly[CsvGenerator[URL]].toColumnName(Some("x"), "y") shouldBe "x.y"
    implicitly[CsvGenerator[Double]].toColumnName(None, "x") shouldBe "x"
    implicitly[CsvGenerator[Boolean]].toColumnName(Some("x"), "y") shouldBe "x.y"
  }

  it should "generate header for an Option[Int]" in {
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    implicit val optionIntGenerator: CsvGenerator[Option[Int]] = new CsvGenerators {}.optionGenerator
    optionIntGenerator.toColumnName(None, "x") shouldBe "x"
    implicitly[CsvGenerator[Option[Int]]].toColumnName(Some("x"), "y") shouldBe "x.y"
  }

  it should "generate header for Sequence" in {
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    implicit val seqIntGenerator: CsvGenerator[Seq[Int]] = new CsvGenerators {}.sequenceGenerator
    seqIntGenerator.toColumnName(None, "x") shouldBe "x"
    implicitly[CsvGenerator[Seq[Int]]].toColumnName(Some("x"), "y") shouldBe "x.y"
  }

  it should "render 1-tuple" in {
    import CsvGenerators._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    case class Onesy(x: Int)
    implicit val onesyGenerator: CsvGenerator[Onesy] = new CsvGenerators {}.generator1(Onesy)
    onesyGenerator.toColumnName(Some("x"), "y") shouldBe "x.y.x"
  }

  it should "render 6-tuple" in {
    import CsvGenerators._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    case class Sixsy(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int)
    implicit val sixsyGenerator: CsvGenerator[Sixsy] = new CsvGenerators {}.generator6(Sixsy)
    sixsyGenerator.toColumnName(Some("x"), "y") shouldBe "x.y.a, x.y.b, x.y.c, x.y.d, x.y.e, x.y.f"
  }

  it should "render 7-tuple" in {
    import CsvGenerators._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    case class Sevensy(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int)
    implicit val sevensyGenerator: CsvGenerator[Sevensy] = new CsvGenerators {}.generator7(Sevensy)
    sevensyGenerator.toColumnName(Some("x"), "y") shouldBe "x.y.a, x.y.b, x.y.c, x.y.d, x.y.e, x.y.f, x.y.g"
  }

  it should "render 8-tuple" in {
    import CsvGenerators._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    case class Eightsy(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int)
    implicit val EightsyGenerator: CsvGenerator[Eightsy] = new CsvGenerators {}.generator8(Eightsy)
    EightsyGenerator.toColumnName(Some("x"), "y") shouldBe "x.y.a, x.y.b, x.y.c, x.y.d, x.y.e, x.y.f, x.y.g, x.y.h"
  }

  it should "render 9-tuple" in {
    import CsvGenerators._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    case class Ninesy(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int)
    implicit val NinesyGenerator: CsvGenerator[Ninesy] = new CsvGenerators {}.generator9(Ninesy)
    NinesyGenerator.toColumnName(Some("x"), "y") shouldBe "x.y.a, x.y.b, x.y.c, x.y.d, x.y.e, x.y.f, x.y.g, x.y.h, x.y.i"
  }

  it should "render 10-tuple" in {
    import CsvGenerators._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    case class Tensy(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int)
    implicit val TensyGenerator: CsvGenerator[Tensy] = new CsvGenerators {}.generator10(Tensy)
    TensyGenerator.toColumnName(Some("x"), "y") shouldBe "x.y.a, x.y.b, x.y.c, x.y.d, x.y.e, x.y.f, x.y.g, x.y.h, x.y.i, x.y.j"
  }

  it should "render 11-tuple" in {
    import CsvGenerators._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    case class Elevensy(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int)
    implicit val ElevensyGenerator: CsvGenerator[Elevensy] = new CsvGenerators {}.generator11(Elevensy)
    ElevensyGenerator.toColumnName(Some("x"), "y") shouldBe "x.y.a, x.y.b, x.y.c, x.y.d, x.y.e, x.y.f, x.y.g, x.y.h, x.y.i, x.y.j, x.y.k"
  }

  it should "render 12-tuple" in {
    import CsvGenerators._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    case class Twelvesy(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int, j: Int, k: Int, l: Int)
    implicit val TwelvesyGenerator: CsvGenerator[Twelvesy] = new CsvGenerators {}.generator12(Twelvesy)
    TwelvesyGenerator.toColumnName(Some("x"), "y") shouldBe "x.y.a, x.y.b, x.y.c, x.y.d, x.y.e, x.y.f, x.y.g, x.y.h, x.y.i, x.y.j, x.y.k, x.y.l"
  }


  behavior of "CsvRenderers"

  it should "render a RawRow" in {
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    implicit val renderer: CsvRenderer[RawRow] = new CsvRenderers {}.rawRowRenderer
    val rawRow: RawRow = RawRow(Seq("42", "HGTTG"), Header.create("answer", "title"))
    renderer.render(rawRow) shouldBe "42, HGTTG"
  }

  it should "render an Option[Int]" in {
    import CsvRenderers._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    implicit val optionIntRenderer: CsvRenderer[Option[Int]] = new CsvRenderers {}.optionRenderer
    optionIntRenderer.render(Some(42)) shouldBe "42"
    optionIntRenderer.render(None) shouldBe ""
  }

  it should "render a Sequence" in {
    import CsvRenderers._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    implicit val seqIntRenderer: CsvRenderer[Seq[Long]] = new CsvRenderers {}.sequenceRenderer
    seqIntRenderer.render(Seq(52L, 85L)) shouldBe "52, 85"
  }

  it should "render 1-tuple" in {
    import CsvRenderers._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    case class Onesy(x: Int)
    implicit val onesyCsvRenderer: CsvRenderer[Onesy] = new CsvRenderers {}.renderer1(Onesy)
    onesyCsvRenderer.render(Onesy(42)) shouldBe "42"
  }

  it should "render an IntPair" in {
    import CsvRenderers._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    implicit val intPairCsvRenderer: CsvRenderer[IntPair] = new CsvRenderers {}.renderer2(IntPair.apply)
    val intPair = IntPair(42, 99)
    intPairCsvRenderer.render(intPair) shouldBe "42, 99"
  }

  it should "render 6-tuple" in {
    import CsvRenderers._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    case class Sixsy(a: Boolean, b: Boolean, c: Boolean, d: Boolean, e: Boolean, f: Boolean)
    implicit val intPairCsvRenderer: CsvRenderer[Sixsy] = new CsvRenderers {}.renderer6(Sixsy)

    intPairCsvRenderer.render(Sixsy(a = true, b = false, c = true, d = false, e = true, f = false)) shouldBe "true, false, true, false, true, false"
  }

  it should "render 7-tuple" in {
    import CsvRenderers._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    case class Sevensy(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double, g: Double)
    implicit val intPairCsvRenderer: CsvRenderer[Sevensy] = new CsvRenderers {}.renderer7(Sevensy)

    intPairCsvRenderer.render(Sevensy(1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2)) shouldBe "1.2, 2.2, 3.2, 4.2, 5.2, 6.2, 7.2"
  }

  it should "render 8-tuple" in {
    import CsvRenderers._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    case class Eightsy(a: URL, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int)
    implicit val intPairCsvRenderer: CsvRenderer[Eightsy] = new CsvRenderers {}.renderer8(Eightsy)

    intPairCsvRenderer.render(Eightsy(new URL("https://google.com"), 2, 3, 4, 5, 6, 7, 8)) shouldBe "https://google.com, 2, 3, 4, 5, 6, 7, 8"
  }

  it should "render 9-tuple" in {
    import CsvRenderers._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    case class Ninesy(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, j: Int)
    implicit val intPairCsvRenderer: CsvRenderer[Ninesy] = new CsvRenderers {}.renderer9(Ninesy)

    intPairCsvRenderer.render(Ninesy(1, 2, 3, 4, 5, 6, 7, 8, 9)) shouldBe "1, 2, 3, 4, 5, 6, 7, 8, 9"
  }

  it should "render 10-tuple" in {
    import CsvRenderers._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    case class Tensy(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, j: Int, k: Int)
    implicit val intPairCsvRenderer: CsvRenderer[Tensy] = new CsvRenderers {}.renderer10(Tensy)

    intPairCsvRenderer.render(Tensy(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) shouldBe "1, 2, 3, 4, 5, 6, 7, 8, 9, 10"
  }

  it should "render 11-tuple" in {
    import CsvRenderers._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    case class Elevensy(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, j: Int, k: Int, l: Int)
    implicit val intPairCsvRenderer: CsvRenderer[Elevensy] = new CsvRenderers {}.renderer11(Elevensy)

    intPairCsvRenderer.render(Elevensy(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)) shouldBe "1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11"
  }

  behavior of "CsvTableStringRenderer"

  it should "render a table 1" in {
    val csvRenderers = new CsvRenderers {}
    import CsvRenderers._
    val csvGenerators = new CsvGenerators {}
    import CsvGenerators._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    implicit val intPairCsvRenderer: CsvRenderer[IntPair] = csvRenderers.renderer2(IntPair.apply)
    implicit val intPairCsvGenerator: CsvProductGenerator[IntPair] = csvGenerators.generator2(IntPair.apply)
    import IntPair._
    val tableIO = Table.parseFile("src/test/resources/com/phasmidsoftware/table/intPairs.csv")
    val resultIO = tableIO flatMap {
      case iIt@HeadedTable(_, _) => CsvTableStringRenderer[IntPair]().render(iIt)
    }
    EvaluateIO(resultIO).toString shouldBe "a, b\n1, 2\n42, 99\n"
  }

  case class Hawks(bw: Int, rt: Int) {
    def map(f: Int => Int): Hawks = Hawks(f(bw), f(rt))
  }

  object Hawks {

    object HawksParser extends JavaTokenParsers {
      lazy val pair: Parser[(Int, Int)] = wholeNumber ~ wholeNumber ^^ { case x ~ y => (x.toInt, y.toInt) }
    }

    trait HawksRowParser extends StringParser[Hawks] {
      def parse(indexedString: (String, Int))(header: Header): Try[Hawks] = HawksParser.parseAll(HawksParser.pair, indexedString._1) match {
        case HawksParser.Success((x, y), _) => Success(Hawks(x, y))
        case _ => Failure(TableException(s"unable to parse ${indexedString._1}"))
      }

      //noinspection NotImplementedCode
      def parseHeader(w: Seq[String]): IO[Header] = ???
    }

    implicit object HawksRowParser extends HawksRowParser

    trait HawksTableParser extends StringTableParser[Table[Hawks]] {
      type Row = Hawks

      val maybeFixedHeader: Option[Header] = Some(Header.create("a", "b"))

      val headerRowsToRead: Int = 0

      protected def builder(rows: Iterable[Hawks], header: Header): Table[Hawks] = HeadedTable(rows, Header[Hawks]())

      val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]
    }

    implicit object HawksTableParser extends HawksTableParser
  }

  case class DailyRaptorReport(date: LocalDate, weather: String, hawks: Hawks)

  object DailyRaptorReport {

    object DailyRaptorReportParser extends CellParsers {
      private val raptorReportDateFormatter = DateTimeFormat.forPattern("MM/dd/yyyy")

      def parseDate(w: String): LocalDate = LocalDate.parse(w, raptorReportDateFormatter)

      implicit val dateParser: CellParser[LocalDate] = cellParser(parseDate)
      implicit val dailyRaptorReportColumnHelper: ColumnHelper[DailyRaptorReport] = columnHelper()
      implicit val hawksCellParser: CellParser[Hawks] = cellParser2(Hawks.apply)
      implicit val dailyRaptorReportParser: CellParser[DailyRaptorReport] = cellParser3(DailyRaptorReport.apply)
    }

    import DailyRaptorReportParser._

    trait DailyRaptorReportConfig extends DefaultRowConfig {
      override val string: Regex = """[\w/\- ]+""".r
      override val delimiter: Regex = """\t""".r
    }

    implicit object DailyRaptorReportConfig extends DailyRaptorReportConfig

    implicit val parser: StandardRowParser[DailyRaptorReport] = StandardRowParser[DailyRaptorReport](LineParser.apply)

    trait DailyRaptorReportTableParser extends StringTableParser[Table[DailyRaptorReport]] {
      type Row = DailyRaptorReport

      val maybeFixedHeader: Option[Header] = None

      val headerRowsToRead: Int = 1

      val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]

      protected def builder(rows: Iterable[DailyRaptorReport], header: Header): Table[Row] = HeadedTable(rows, header)
    }

    implicit object DailyRaptorReportTableParser extends DailyRaptorReportTableParser
  }

  it should "parse and output raptors from raptors.csv" in {
    import DailyRaptorReport._

    val xio = for (r <- Table.parseResource(classOf[TableParserSpec].getResource("/raptors.csv"))) yield r
    val resultIO = xio flatMap {
      case rt@HeadedTable(_, _) =>
        rt.rows.size shouldBe 13
        import CsvGenerators._
        import CsvRenderers._
        implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
        implicit val intPairCsvRenderer: CsvRenderer[Hawks] = new CsvRenderers {}.renderer2(Hawks.apply)
        implicit val intPairCsvGenerator: CsvProductGenerator[Hawks] = new CsvGenerators {}.generator2(Hawks.apply)
        implicit val dateCsvRenderer: CsvRenderer[LocalDate] = new CsvRenderer[LocalDate] {
          val csvAttributes: CsvAttributes = implicitly[CsvAttributes]

          def render(t: LocalDate, attrs: Map[String, String]): String = t.toString
        }
        implicit val dateCsvGenerator: CsvGenerator[LocalDate] = new StandardCsvGenerator[LocalDate]
        implicit val DRRCsvRenderer: CsvRenderer[DailyRaptorReport] = new CsvRenderers {}.renderer3(DailyRaptorReport.apply)
        implicit val DRRCsvGenerator: CsvProductGenerator[DailyRaptorReport] = new CsvGenerators {}.generator3(DailyRaptorReport.apply)
        rt.toCSV
    }
    val expected =
      """date, weather, hawks.bw, hawks.rt
        |2018-09-12, Dense Fog/Light Rain, 0, 0
        |2018-09-13, Fog/Overcast, 79, 0
        |2018-09-14, Drizzle/Fog/Overcast, 1, 0
        |2018-09-15, Overcast/ Mostly Cloudy, 1054, 0
        |2018-09-16, Partly Cloudy, 3308, 5
        |2018-09-17, Dense Fog/Light Rain, 0, 0
        |2018-09-18, Clear/Partly cloudy, 260, 0
        |2018-09-19, Overcast/Mostly cloudy/Partly cloudy/Clear, 821, 4
        |2018-09-20, Overcast/Fog, 36, 1
        |2018-09-21, Dense Fog/Overcast, 29, 0
        |2018-09-22, Partly cloudy/Mostly cloudy/Overcast, 455, 3
        |2018-09-23, Overcast, 470, 2
        |2018-09-24, Overcast/Mostly cloudy, 292, 2
        |""".stripMargin
    matchIO(resultIO) {
      case `expected` => succeed
    }

  }

  case class WeatherHawks(weather: String, hawks: Hawks)

  case class NestedRaptorReport(date: LocalDate, weatherHawks: WeatherHawks)

  object NestedRaptorReport {

    object NestedRaptorReportParser extends CellParsers {
      private val raptorReportDateFormatter = DateTimeFormat.forPattern("MM/dd/yyyy")

      def parseDate(w: String): LocalDate = LocalDate.parse(w, raptorReportDateFormatter)

      implicit val dateParser: CellParser[LocalDate] = cellParser(parseDate)
      implicit val dailyRaptorReportColumnHelper: ColumnHelper[NestedRaptorReport] = columnHelper()
      implicit val hawksCellParser: CellParser[Hawks] = cellParser2(Hawks.apply)
      implicit val weatherHawksCellParser: CellParser[WeatherHawks] = cellParser2(WeatherHawks.apply)
      implicit val dailyRaptorReportParser: CellParser[NestedRaptorReport] = cellParser2(NestedRaptorReport.apply)
    }

    import NestedRaptorReportParser._

    trait NestedRaptorReportConfig extends DefaultRowConfig {
      override val string: Regex = """[\w/\- ]+""".r
      override val delimiter: Regex = """\t""".r
    }

    implicit object NestedRaptorReportConfig extends NestedRaptorReportConfig

    implicit val parser: StandardRowParser[NestedRaptorReport] = StandardRowParser[NestedRaptorReport](LineParser.apply)

    trait NestedRaptorReportTableParser extends StringTableParser[Table[NestedRaptorReport]] {
      type Row = NestedRaptorReport

      val maybeFixedHeader: Option[Header] = None

      val headerRowsToRead: Int = 1

      val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]

      protected def builder(rows: Iterable[NestedRaptorReport], header: Header): Table[Row] = HeadedTable(rows, header)
    }

    implicit object NestedRaptorReportTableParser extends NestedRaptorReportTableParser
  }

  it should "parse and output raptors from raptors.csv with a more nested type" in {
    import NestedRaptorReport._

    val xio = for (r <- Table.parseResource(classOf[TableParserSpec].getResource("/raptors.csv"))) yield r
    val resultIO = xio flatMap {
      case rt@HeadedTable(_, _) =>
        rt.rows.size shouldBe 13
        import CsvGenerators._
        import CsvRenderers._
        implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
        implicit val hawksCsvRenderer: CsvRenderer[Hawks] = new CsvRenderers {}.renderer2(Hawks.apply)
        implicit val hawksCsvGenerator: CsvProductGenerator[Hawks] = new CsvGenerators {}.generator2(Hawks.apply)
        implicit val weatherHawksCsvRenderer: CsvRenderer[WeatherHawks] = new CsvRenderers {}.renderer2(WeatherHawks.apply)
        implicit val weatherHawksCsvGenerator: CsvProductGenerator[WeatherHawks] = new CsvGenerators {}.generator2(WeatherHawks.apply)
        implicit val dateCsvRenderer: CsvRenderer[LocalDate] = new CsvRenderer[LocalDate] {
          val csvAttributes: CsvAttributes = implicitly[CsvAttributes]

          def render(t: LocalDate, attrs: Map[String, String]): String = t.toString
        }
        implicit val dateCsvGenerator: CsvGenerator[LocalDate] = new StandardCsvGenerator[LocalDate]
        implicit val DRRCsvRenderer: CsvRenderer[NestedRaptorReport] = new CsvRenderers {}.renderer2(NestedRaptorReport.apply)
        implicit val DRRCsvGenerator: CsvProductGenerator[NestedRaptorReport] = new CsvGenerators {}.generator2(NestedRaptorReport.apply)
        rt.take(1).toCSV
    }
    EvaluateIO(resultIO) shouldBe "date, weatherHawks.weather, weatherHawks.hawks.bw, weatherHawks.hawks.rt\n2018-09-12, Dense Fog/Light Rain, 0, 0\n"
  }
}
