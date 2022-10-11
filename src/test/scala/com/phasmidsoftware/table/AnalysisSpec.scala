package com.phasmidsoftware.table

import cats.effect.IO
import com.phasmidsoftware.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.util.CheckIO
import com.phasmidsoftware.util.FP.resource
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}

import scala.io.Source

class AnalysisSpec extends AnyFlatSpec with Matchers {

  behavior of "Analysis"

  /**
    * NOTE: it is perfectly proper for there to be a number of parsing problems.
    * These are application-specific and are not indicative of any bugs in the
    * TableParser library itself.
    */
  it should "be correct for airbnb2.csv" in {
    val airBNBFile = "/airbnb2.csv"
    val parser = RawTableParser(TableParser.includeAll, None, forgiving = true).setMultiline(true)
    val sy: IO[Source] = IO.fromTry(resource[AnalysisSpec](airBNBFile) map Source.fromURL)
    CheckIO.checkResultIO(parser parse sy, Timeout(Span(2, Seconds))) {
      case t@HeadedTable(_, _) =>
        val analysis = Analysis(t)
        analysis.rows shouldBe 254
        analysis.columns shouldBe 87
        analysis.columnMap.size shouldBe 87
        analysis.columnMap("bedrooms") should matchPattern { case Column("Int", false, _) => }
        analysis.columnMap("accommodates").toString shouldBe "Int (range: 1.0-10.0, mean: 2.783464566929134, stdDev: 1.7670324685210184)"
        analysis.columnMap("license").toString shouldBe "optional Int"
    }
  }

}
