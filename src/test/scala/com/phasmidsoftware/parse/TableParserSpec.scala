package com.phasmidsoftware.parse

import java.util.Date

import com.phasmidsoftware.table.{Header, Table, TableException, TableWithoutHeader}
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.{FlatSpec, Matchers}

import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}

class TableParserSpec extends FlatSpec with Matchers {

  behavior of "TableParser"

  case class IntPair(a: Int, b: Int)

  object IntPair {

    class IntPairParser extends JavaTokenParsers {
      def pair: Parser[(Int, Int)] = wholeNumber ~ wholeNumber ^^ { case x ~ y => (x.toInt, y.toInt) }
    }

    val intPairParser = new IntPairParser

    trait IntPairRowParser extends RowParser[IntPair] {
      override def parse(w: String)(header: Header): Try[IntPair] = intPairParser.parseAll(intPairParser.pair, w) match {
        case intPairParser.Success((x, y), _) => Success(IntPair(x, y))
        case _ => Failure(TableException(s"unable to parse $w"))
      }

      //noinspection NotImplementedCode
      override def parseHeader(w: String): Try[Header] = ???
    }

    implicit object IntPairRowParser extends IntPairRowParser

    trait IntPairTableParser extends TableParser[Table[IntPair]] {
      type Row = IntPair

      def hasHeader: Boolean = false

      def rowParser: RowParser[Row] = implicitly[RowParser[Row]]

      def builder(rows: Seq[Row]): Table[IntPair] = TableWithoutHeader(rows)
    }

    implicit object IntPairTableParser extends IntPairTableParser

  }

  it should "parse int pair" in {

    import IntPair._

    val strings: Seq[String] = Seq("1 2")
    Table.parse(strings) match {
      case Success(_) => succeed
      case Failure(x) => fail(x.getLocalizedMessage)
    }
  }

  behavior of "TableParser with StandardRowParser"

  case class DailyRaptorReport(date: LocalDate, weather: String, bw: Int, rt: Int)

  object DailyRaptorReport {
    val header: Seq[String] = Seq("date", "weather", "bw", "ri")

    object DailyRaptorReportParser extends CellParsers {

      private val raptorReportDateFormatter = DateTimeFormat.forPattern("MM/dd/yyyy")

      def parseDate(w: String): LocalDate = LocalDate.parse(w, raptorReportDateFormatter)

      implicit val dateParser: CellParser[LocalDate] = cellParser(parseDate)
      implicit val dailyRaptorReportColumnHelper: ColumnHelper[DailyRaptorReport] = columnHelper()
      implicit val dailyRaptorReportParser: CellParser[DailyRaptorReport] = cellParser4(DailyRaptorReport.apply)
    }

    import DailyRaptorReportParser._

    trait DailyRaptorReportConfig extends DefaultRowConfig {
      override val string: Regex = """[\w\/\-\ ]+""".r
      override val delimiter: Regex = """\t""".r
    }

    implicit object DailyRaptorReportConfig extends DailyRaptorReportConfig

    implicit val parser: StandardRowParser[DailyRaptorReport] = StandardRowParser[DailyRaptorReport](LineParser.apply)

    trait DailyRaptorReportTableParser extends TableParser[Table[DailyRaptorReport]] {
      type Row = DailyRaptorReport

      def hasHeader: Boolean = true

      def rowParser: RowParser[Row] = implicitly[RowParser[Row]]

      def builder(rows: Seq[Row]): Table[DailyRaptorReport] = TableWithoutHeader(rows)
    }

    implicit object DailyRaptorReportTableParser extends DailyRaptorReportTableParser

  }

  behavior of "RowParser.parse"

  it should "parse regex string" in {
    import DailyRaptorReport._

    val rowParser = implicitly[RowParser[DailyRaptorReport]]
    val firstRow = "Date\tWeather\tWnd Dir\tWnd Spd\tBV\tTV\tUV\tOS\tBE\tNH\tSS\tCH\tGO\tUA\tRS\tBW\tRT\tRL\tUB\tGE\tUE\tAK\tM\tP\tUF\tUR\tOth\tTot"
    val row = "09/16/2018\tPartly Cloudy\tSE\t6-12\t0\t0\t0\t4\t19\t3\t30\t2\t0\t0\t2\t3308\t5\t0\t0\t0\t0\t27\t8\t1\t0\t1\t0\t3410"
    val Success(header) = rowParser.parseHeader(firstRow)

    val hawkCount: Try[DailyRaptorReport] = parser.parse(row)(header)
    hawkCount should matchPattern { case Success(DailyRaptorReport(_, "Partly Cloudy", 3308, 5)) => }
  }

  behavior of "Table.parse"

  it should "parse raptors from raptors.csv" in {
    import DailyRaptorReport._

    val x: Try[Table[DailyRaptorReport]] = for (r <- Table.parse(classOf[TableParserSpec].getResource("/raptors.csv"))) yield r
    x should matchPattern { case Success(TableWithoutHeader(_)) => }
    x.get.rows.size shouldBe 13
    //noinspection ScalaDeprecation
    x.get.rows.head shouldBe DailyRaptorReport(LocalDate.fromDateFields(new Date(118, 8, 12)), "Dense Fog/Light Rain", 0, 0)
  }

  it should "parse raptors from Seq[String]" in {
    import DailyRaptorReport._

    val raw = Seq("Date\tWeather\tWnd Dir\tWnd Spd\tBV\tTV\tUV\tOS\tBE\tNH\tSS\tCH\tGO\tUA\tRS\tBW\tRT\tRL\tUB\tGE\tUE\tAK\tM\tP\tUF\tUR\tOth\tTot",
      "09/16/2018\tPartly Cloudy\tSE\t6-12\t0\t0\t0\t4\t19\t3\t30\t2\t0\t0\t2\t3308\t5\t0\t0\t0\t0\t27\t8\t1\t0\t1\t0\t3410",
      "09/19/2018\tOvercast/Mostly cloudy/Partly cloudy/Clear\tNW\t4-7\t0\t0\t0\t47\t12\t0\t84\t10\t0\t0\t1\t821\t4\t0\t1\t0\t0\t27\t4\t1\t0\t2\t0\t1014")
    val x = for (r <- Table.parse(raw)) yield r
    x should matchPattern { case Success(TableWithoutHeader(_)) => }
    x.get.rows.size shouldBe 2
    //noinspection ScalaDeprecation
    x.get.rows.head shouldBe DailyRaptorReport(LocalDate.fromDateFields(new Date(118, 8, 16)), "Partly Cloudy", 3308, 5)

  }

}
