package com.phasmidsoftware.table

import cats.effect.IO
import com.phasmidsoftware.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.util.EvaluateIO.matchIO
import com.phasmidsoftware.util.FP.resource
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import scala.io.Source

class AirBNBSpec extends AnyFlatSpec with Matchers {

  behavior of "AirBNB table"

  it should "be ingested properly" in {
    val airBNBFile = "/airbnb2.csv"

    // Set up the source
    val sy: IO[Source] = IO.fromTry(for (u <- resource[AirBNBSpec](airBNBFile)) yield Source.fromURL(u))

    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser: RawTableParser = RawTableParser().setPredicate(TableParser.sampler(2)).setMultiline(true)

    // Create the table
    val wsty: IO[RawTable] = parser.parse(sy)

    matchIO(wsty, Timeout(Span(4, Seconds))) {
      case t@HeadedTable(r, _) =>
        val analysis = Analysis(t)
        println(s"AirBNB: $analysis")
        analysis.rows shouldBe 127 +- 32
        r take 100 foreach println
        succeed
    }
  }
}
