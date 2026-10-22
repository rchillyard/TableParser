package com.phasmidsoftware.table

import cats.effect.IO
import com.phasmidsoftware.examples.crime.CrimeLocation
import com.phasmidsoftware.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.table.Column.make
import com.phasmidsoftware.util.EvaluateIO.matchIO
import com.phasmidsoftware.util.FP.{resource, sequence}
import com.phasmidsoftware.util.{EvaluateIO, FP}
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import scala.io.Source

class AnalysisFuncSpec extends AnyFlatSpec with Matchers {
  implicit val parser: RawTableParser = RawTableParser(TableParser.includeAll, None, forgiving = true).setMultiline(true)

  behavior of "Analysis (functional specs)"

  it should "analyze the complete crime file" in {
    val crimeFile = "../examples/crime/2023-01-metropolitan-street.csv"

    implicit object validityRawRow extends Validity[RawRow] {
      def isValid(r: RawRow): Boolean = ! {
        val latitude: Double = r("latitude").get.toDoubleOption.getOrElse(55)
        val longitude: Double = r("longitude").get.toDoubleOption.getOrElse(1)
        val lsoaCode = r("LSOA code").getOrElse("")
        CrimeLocation.isValid(longitude, latitude, lsoaCode)
      }
    }

    // Set up the source
    val sy: IO[Source] = IO.fromTry(for (u <- FP.resource[Analysis](crimeFile)) yield Source.fromURL(u))

    val fraction = 1
    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser: RawTableParser = RawTableParser().setPredicate(TableParser.sampler(fraction))

    EvaluateIO.check(parser.parse(sy), Timeout(Span(10, Seconds))) {
      case t@HeadedTable(r, _) =>
        val q = t.filterValid
        Analysis(q) match {
          case a@Analysis(_, 12, _) =>
            println(s"Crime analysis: $a")
            r take 10 foreach println
          case _ =>
            println(s"Not good analysis")
            fail("didnt match")
        }
    }
  }
}
