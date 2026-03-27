package com.phasmidsoftware.tableparser.zio.table

import com.phasmidsoftware.tableparser.core.examples.crime.Crime
import com.phasmidsoftware.tableparser.core.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.tableparser.core.table.Column.make
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.util.FP
import com.phasmidsoftware.tableparser.core.util.FP.{resource, sequence}
import com.phasmidsoftware.tableparser.zio.util.EvaluateZIO.matchZIO
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source
import scala.util.Try
import zio._

class AnalysisSpec extends AnyFlatSpec with Matchers {

  val airBNBFile = "airbnb2.csv"
  implicit val parser: RawTableParser = RawTableParser(TableParser.includeAll, None, forgiving = true).setMultiline(true)

  behavior of "Analysis"

  /**
   * NOTE: it is perfectly proper for there to be a number of parsing problems.
   * These are application-specific and are not indicative of any bugs in the
   * TableParser library itself.
   */
  it should "be correct for airbnb2.csv" in {
    val sy: Try[Source] = (resource[AnalysisSpec](airBNBFile) map Source.fromURL)
    matchZIO(ZIO.fromTry(parser parse sy)) {
      case t@HeadedTable(_, _) =>
        val analysis = Analysis(t)
        analysis.rows == 253 &&
                analysis.columns == 87 &&
                analysis.columnMap.size == 87 &&
                (analysis.columnMap("bedrooms") match {
                  case Column("Int", false, _) => true;
                  case _ => false
                }) &&
                analysis.columnMap("accommodates").toString.startsWith("Int (range: 1.0-10.0, mean: 2.7") &&
                analysis.columnMap("license").toString == "optional Int"
    }
  }

  behavior of "Column"

  it should "make" in {
    val ti: Task[RawTable] = ZIO.fromTry(Table.parseResource(airBNBFile, classOf[AnalysisSpec]))
    matchZIO(ti) {
      case t: RawTable =>
        val z: Iterator[Option[String]] = t.column("accommodates")
        val maybeColumn: Option[Column] = sequence(z) flatMap (ws => make(ws))
        maybeColumn should matchPattern { case Some(Column(_, _, Some(_))) => }
        maybeColumn match {
          case Some(Column("Int", false, Some(maybeStatistics))) =>
            maybeStatistics.getStatistics() match {
              case Some(statistics) =>
                statistics.mu shouldBe 2.785 +- 0.005
              case None => fail("Statistics should be available")
            }
          case x => fail(x.toString)
        }
        true
    }
  }

  behavior of "Main"

  it should "doMain" in {
    Main.doMain(FP.resource[Crime]("2023-01-metropolitan-street-sample.csv"))
  }
}
