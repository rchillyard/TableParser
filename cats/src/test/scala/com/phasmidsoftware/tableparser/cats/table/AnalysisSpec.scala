package com.phasmidsoftware.tableparser.cats.table

import cats.effect.IO
import com.phasmidsoftware.tableparser.cats.util.EvaluateIO.matchIO
import com.phasmidsoftware.tableparser.core.examples.crime.Crime
import com.phasmidsoftware.tableparser.core.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.tableparser.core.table.Column.make
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.util.FP
import com.phasmidsoftware.tableparser.core.util.FP.{resource, sequence}
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import scala.io.Source
import scala.util.Try

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
    matchIO(IO.fromTry(parser parse sy), Timeout(Span(2, Seconds))) {
      case t@HeadedTable(_, _) =>
        val analysis = Analysis(t)
        analysis.rows shouldBe 253
        analysis.columns shouldBe 87
        analysis.columnMap.size shouldBe 87
        analysis.columnMap("bedrooms") should matchPattern { case Column("Int", false, _) => }
        analysis.columnMap("accommodates").toString should startWith("Int (range: 1.0-10.0, mean: 2.7")
        analysis.columnMap("license").toString shouldBe "optional Int"
    }
  }

  behavior of "Column"

  it should "make" in {
    val ti: IO[RawTable] = IO.fromTry(Table.parseResource(airBNBFile, classOf[AnalysisSpec]))
    matchIO(ti) {
      case t: RawTable =>
        val z: Iterator[Option[String]] = t.column("accommodates")
        val maybeColumn: Option[Column] = sequence(z) flatMap (ws => make(ws))
        maybeColumn should matchPattern { case Some(Column(_, _, Some(_))) => }
        maybeColumn.get match {
          case Column("Int", false, Some(maybeStatistics)) =>
            maybeStatistics match {
              case EagerStatistics(stats) =>
                stats.mu shouldBe 2.785 +- 0.005
              case LazyStatistics(_) =>
                fail("should not be lazy")
            }
          case x => fail(x.toString)
        }
    }
  }

  behavior of "Histogram"

  it should "make a histogram"

  behavior of "Main"

  it should "doMain" in {
    Main.doMain(FP.resource[Crime]("2023-01-metropolitan-street-sample.csv"))
  }
}
