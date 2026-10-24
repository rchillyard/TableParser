package com.phasmidsoftware.tableparser.cats.table

import cats.effect.IO
import com.phasmidsoftware.tableparser.cats.util.EvaluateIO
import com.phasmidsoftware.tableparser.core.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.util.FP
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import scala.io.Source
import scala.util.Try

class AnalysisFuncSpec extends AnyFlatSpec with Matchers:

  behavior of "Analysis (functional specs)"

  it should "analyze the complete crime file" in:
    val crimeFile = "../examples/crime/2023-01-metropolitan-street.csv"

    // A row is valid if it has a location.
    implicit object validityRawRow extends Validity[RawRow]:
      def isValid(r: RawRow): Boolean =
        r("latitude").toOption.flatMap(_.toDoubleOption).isDefined && r("longitude").toOption.flatMap(_.toDoubleOption).isDefined

    // Set up the source
    val sy: Try[Source] = for (u <- FP.resource[ColumnStatistics](crimeFile)) yield Source.fromURL(u)

    val fraction = 1
    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser: RawTableParser = RawTableParser().setPredicate(TableParser.sampler(fraction))

    EvaluateIO.check(IO.fromTry(parser.parse(sy)), Timeout(Span(10, Seconds))):
      case t@HeadedTable(r, _) =>
        val valid = t.filterValid
        valid.size should be < t.size
        val analysis = Analysis(valid)
        analysis.columns shouldBe 12
        println(s"Crime analysis: $analysis")
        r take 10 foreach println
