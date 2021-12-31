package com.phasmidsoftware.render

import com.phasmidsoftware.parse._
import com.phasmidsoftware.table._
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.File
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}

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
      def parseHeader(w: Seq[String]): Try[Header] = ???
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
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    implicitly[CsvGenerator[Int]].toColumnName(None, "x") shouldBe "x"
    implicitly[CsvGenerator[Int]].toColumnName(Some("x"), "y") shouldBe "x.y"
  }

  it should "generate header for an Option[Int]" in {
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    implicit val optionIntGenerator: CsvGenerator[Option[Int]] = new CsvGenerators {}.optionGenerator
    optionIntGenerator.toColumnName(None, "x") shouldBe "x"
    implicitly[CsvGenerator[Option[Int]]].toColumnName(Some("x"), "y") shouldBe "x.y"
  }

  behavior of "CsvRenderers"

  it should "render an Option[Int]" in {
    import CsvRenderers._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    implicit val optionIntRenderer: CsvRenderer[Option[Int]] = new CsvRenderers {}.optionRenderer
    optionIntRenderer.render(Some(42)) shouldBe "42"
    optionIntRenderer.render(None) shouldBe ""
  }

  it should "render an 1-tuple" in {
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

  behavior of "CsvTableRenderer"

  it should "render a table 1" in {
    val csvRenderers = new CsvRenderers {}
    import CsvRenderers._
    val csvGenerators = new CsvGenerators {}
    import CsvGenerators._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    implicit val intPairCsvRenderer: CsvRenderer[IntPair] = csvRenderers.renderer2(IntPair.apply)
    implicit val intPairCsvGenerator: CsvProductGenerator[IntPair] = csvGenerators.generator2(IntPair.apply)
    import IntPair._
    val iIty = Table.parseFile(new File("src/test/resources/com/phasmidsoftware/table/intPairs.csv"))
    iIty should matchPattern { case Success(_) => }
    val iIt = iIty.get
    val ws = CsvTableRenderer[IntPair]().render(iIt)
    ws.head shouldBe "a, b"
    ws.tail.head shouldBe "1, 2"
    ws.tail.tail.head shouldBe "42, 99"
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
      def parseHeader(w: Seq[String]): Try[Header] = ???
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

    val rty: Try[Table[DailyRaptorReport]] = for (r <- Table.parseResource(classOf[TableParserSpec].getResource("/raptors.csv"))) yield r
    rty should matchPattern { case Success(HeadedTable(_, _)) => }
    val rt = rty.get
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
    implicit val dateCsvGenerator: CsvGenerator[LocalDate] = new BaseCsvGenerator[LocalDate]
    implicit val DRRCsvRenderer: CsvRenderer[DailyRaptorReport] = new CsvRenderers {}.renderer3(DailyRaptorReport.apply)
    implicit val DRRCsvGenerator: CsvProductGenerator[DailyRaptorReport] = new CsvGenerators {}.generator3(DailyRaptorReport.apply)
    val ws: Iterable[String] = rt.toCSV
    ws.head shouldBe "date, weather, hawks.bw, hawks.rt"
    ws.tail.head shouldBe "2018-09-12, Dense Fog/Light Rain, 0, 0"
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

    val rty: Try[Table[NestedRaptorReport]] = for (r <- Table.parseResource(classOf[TableParserSpec].getResource("/raptors.csv"))) yield r
    rty should matchPattern { case Success(HeadedTable(_, _)) => }
    val rt = rty.get
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
    implicit val dateCsvGenerator: CsvGenerator[LocalDate] = new BaseCsvGenerator[LocalDate]
    implicit val DRRCsvRenderer: CsvRenderer[NestedRaptorReport] = new CsvRenderers {}.renderer2(NestedRaptorReport.apply)
    implicit val DRRCsvGenerator: CsvProductGenerator[NestedRaptorReport] = new CsvGenerators {}.generator2(NestedRaptorReport.apply)
    val ws: Iterable[String] = rt.toCSV
    ws.head shouldBe "date, weatherHawks.weather, weatherHawks.hawks.bw, weatherHawks.hawks.rt"
    ws.tail.head shouldBe "2018-09-12, Dense Fog/Light Rain, 0, 0"
  }
}
