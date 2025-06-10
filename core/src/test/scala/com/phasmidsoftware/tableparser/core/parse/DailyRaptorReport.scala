package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.render.{CsvGenerators, CsvProductGenerator, CsvRenderer, CsvRenderers}
import com.phasmidsoftware.tableparser.core.table._
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}

/**
 * Represents a daily report of raptor sightings including the date, weather conditions, and hawk counts.
 * This is just an exemplar for testing parsing, rendering, etc.
 *
 * @param date    The date of the report.
 * @param weather A description of the weather conditions on the day of the report.
 * @param hawks   A case class containing the count of specific types of hawks sighted (e.g., Broad-winged hawks and Red-tailed hawks).
 */
case class DailyRaptorReport(date: LocalDate, weather: String, hawks: Hawks)

/**
 * Provides utility functions and implicit conversions to parse, render, and generate
 * CSV data related to daily raptor sightings. This object includes configurations
 * for string parsing, header handling, and row parsing.
 *
 * Combines various traits such as CellParsers, CsvRenderers, and CsvGenerators for
 * processing CSV data for `DailyRaptorReport` objects.
 */
object DailyRaptorReport extends CellParsers with CsvRenderers with CsvGenerators {

  object DailyRaptorReportParser extends CellParsers with CsvGenerators with CsvRenderers {
    implicit val dateParser: CellParser[LocalDate] = cellParser(LocalDate.parse(_: String, DateTimeFormat.forPattern("MM/dd/yyyy")))
    implicit val helper: ColumnHelper[DailyRaptorReport] = columnHelper()
  }

  import DailyRaptorReportParser._

  implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
  implicit val cellParser: CellParser[DailyRaptorReport] = cellParser3(apply)
  implicit val renderer: CsvRenderer[DailyRaptorReport] = renderer3(apply)
  implicit val csvGenerator: CsvProductGenerator[DailyRaptorReport] = generator3(apply)

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

/**
 * Represents the characteristics of a hawk, defined by two integer attributes.
 *
 * @param bw Integer attribute representing sightings of the Broad-winged hawk.
 * @param rt Integer attribute representing sightings of the Red-tailed hawk.
 */
case class Hawks(bw: Int, rt: Int) {
  def map(f: Int => Int): Hawks = Hawks(f(bw), f(rt))
}

/**
 * Object for handling Hawks data with CSV-related functionality.
 *
 * This object provides implicit implementations for parsing, rendering,
 * generating, and processing Hawks data in CSV format. It includes utilities
 * for working with Hawks rows and tables, enabling flexible and customizable
 * operations on CSV data.
 */
object Hawks extends CsvRenderers with CellParsers with CsvGenerators {

  implicit val parser: CellParser[Hawks] = cellParser2(Hawks.apply)
  implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
  implicit val renderer: CsvRenderer[Hawks] = renderer2(Hawks.apply)
  implicit val csvGenerator: CsvProductGenerator[Hawks] = generator2(Hawks.apply)

  // CONSIDER why do we need this when we have parser (above)
  object HawksParser extends JavaTokenParsers {
    lazy val pair: Parser[(Int, Int)] = wholeNumber ~ wholeNumber ^^ { case x ~ y => (x.toInt, y.toInt) }
  }

  trait HawksRowParser extends StringParser[Hawks] {
    def parse(header: Header)(input: String): Try[Hawks] =
      HawksParser.parseAll(HawksParser.pair, input) match {
        case HawksParser.Success((x, y), _) =>
          Success(Hawks(x, y))
        case _ =>
          Failure(TableException(s"unable to parse $input"))
      }

    def parseIndexed(header: Header)(indexedString: (String, Int)): Try[Hawks] =
      parse(header)(indexedString._1)

    //noinspection NotImplementedCode
    def parseHeader(w: Seq[String]): Try[Header] = Failure(new NotImplementedError("HawksRowParser.parseHeader"))
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

