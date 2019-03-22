package com.phasmidsoftware.tableparser


import com.phasmidsoftware.format.Formats
import org.joda.time.LocalDate
import org.scalatest.{FlatSpec, Matchers}

import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}

class TableParserSpec extends FlatSpec with Matchers {

  case class IntPair(a: Int, b: Int)

  object IntPair {

    class IntPairParser extends JavaTokenParsers {
      def pair: Parser[(Int, Int)] = wholeNumber ~ wholeNumber ^^ { case x ~ y => (x.toInt, y.toInt) }
    }

    val intPairParser = new IntPairParser

    trait IntPairRowParser extends RowParser[IntPair] {
      override def parse(w: String)(header: Seq[String]): Try[IntPair] = intPairParser.parseAll(intPairParser.pair, w) match {
        case intPairParser.Success((x, y), _) => Success(IntPair(x, y))
        case _ => Failure(TableException(s"unable to parse $w"))
      }

      override def parseHeader(w: String): Try[Seq[String]] = ???
    }

    implicit object IntPairRowParser extends IntPairRowParser

    trait IntPairTableParser extends TableParser[Table[IntPair]] {
      type Row = IntPair

      def hasHeader: Boolean = false

      def rowParser: RowParser[Row] = implicitly[RowParser[Row]]

      def builder(rows: Seq[Row]): Table[IntPair] = TableWithoutHeader(rows)
    }

    implicit object IntPairTableParser extends IntPairTableParser {
    }

  }

  behavior of "TableParser"

  it should "parse int pair" in {

    import IntPair._

    val strings: Seq[String] = Seq("1 2")
    Table.parse(strings) match {
      case Success(t) => println(t);
      case Failure(x) => fail(x.getLocalizedMessage)
    }
  }

  behavior of "TableParser with StandardRowParser"

  case class DailyRaptorReport(date: LocalDate, weather: String, bw: Int, rt: Int)

  object DailyRaptorReport {
    val header: Seq[String] = Seq("date", "weather", "bw", "ri")


    object DailyRaptorReportFormat extends Formats {

      import Formats._

      implicit val dailyRaptorReportFormat: CellParser[DailyRaptorReport] = cellReader4(DailyRaptorReport.apply)
    }

    import DailyRaptorReportFormat._

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

    implicit object DailyRaptorReportTableParser extends DailyRaptorReportTableParser {
    }


  }

  behavior of "RowParser"

  it should "parse regex string" in {
    import DailyRaptorReport._

    val rowParser = implicitly[RowParser[DailyRaptorReport]]
    val line1 = "Date\tWeather\tWnd Dir\tWnd Spd\tBV\tTV\tUV\tOS\tBE\tNH\tSS\tCH\tGO\tUA\tRS\tBW\tRT\tRL\tUB\tGE\tUE\tAK\tM\tP\tUF\tUR\tOth\tTot"
    val line2 = "09/16/2018\tPartly Cloudy\tSE\t6-12\t0\t0\t0\t4\t19\t3\t30\t2\t0\t0\t2\t3308\t5\t0\t0\t0\t0\t27\t8\t1\t0\t1\t0\t3410"
    val Success(header) = rowParser.parseHeader(line1)
    val hawkCount: Try[DailyRaptorReport] = parser.parse(line2)(header)
    hawkCount should matchPattern { case Success(DailyRaptorReport(_, "Partly Cloudy", 3308, 5)) => }
  }

  it should "parse raptors.csv" in {
    import DailyRaptorReport._

    //    import DailyRaptorReportTableParser._

    //    val z = implicitly[TableParser[Table[DailyRaptorReport]]]

    val x: Try[Table[DailyRaptorReport]] = for (r <- Table.parse(classOf[TableParserSpec].getResource("/raptors.csv"))) yield r
    x should matchPattern { case Success(_) => }
    println(x)
  }

}
