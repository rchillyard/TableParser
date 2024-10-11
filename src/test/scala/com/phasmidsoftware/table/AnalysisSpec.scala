package com.phasmidsoftware.table

import cats.effect.IO
import com.phasmidsoftware.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.table.Column.make
import com.phasmidsoftware.util.EvaluateIO.matchIO
import com.phasmidsoftware.util.FP.{resource, sequence}
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import scala.io.Source

class AnalysisSpec extends AnyFlatSpec with Matchers {

  val airBNBFile = "/airbnb2.csv"
  implicit val parser: RawTableParser = RawTableParser(TableParser.includeAll, None, forgiving = true).setMultiline(true)

  behavior of "Analysis"

  /**
   * NOTE: it is perfectly proper for there to be a number of parsing problems.
   * These are application-specific and are not indicative of any bugs in the
   * TableParser library itself.
   */
  it should "be correct for airbnb2.csv" in {
    val sy: IO[Source] = IO.fromTry(resource[AnalysisSpec](airBNBFile) map Source.fromURL)
    matchIO(parser parse sy, Timeout(Span(2, Seconds))) {
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
    val ti: IO[RawTable] = Table.parseResource(airBNBFile)
    matchIO(ti) {
      case t: RawTable =>
        val z: Iterator[Option[String]] = t.column("accommodates")
        val maybeColumn: Option[Column] = sequence(z) flatMap (ws => make(ws))
        maybeColumn should matchPattern { case Some(Column(_, _, Some(_))) => }
        maybeColumn.get match {
          case Column("Int", false, Some(statistics)) =>
            statistics.mu shouldBe 2.785 +- 0.005
          case x => fail(x.toString)
        }
    }
  }
}
