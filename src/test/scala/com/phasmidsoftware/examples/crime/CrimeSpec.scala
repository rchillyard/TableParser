package com.phasmidsoftware.examples.crime

import cats.effect.IO
import com.phasmidsoftware.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.table.{Analysis, HeadedTable, RawTable, Table}
import com.phasmidsoftware.util.EvaluateIO.matchIO
import com.phasmidsoftware.util.FP.resource
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import scala.io.Source

class CrimeSpec extends AnyFlatSpec with Matchers {

  behavior of "Crime"
  val crimeFile = "2023-01-metropolitan-street.csv"

  it should "be ingested and analyzed as a RawTable" in {

    // Set up the source
    val sy: IO[Source] = IO.fromTry(for (u <- resource[CrimeSpec](crimeFile)) yield Source.fromURL(u))

    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser: RawTableParser = RawTableParser().setPredicate(TableParser.sampler(2)).setMultiline(true)

    // Create the table
    val wsty: IO[RawTable] = parser.parse(sy)

    matchIO(wsty, Timeout(Span(10, Seconds))) {
      case t@HeadedTable(r, _) =>
        val analysis = Analysis(t)
        println(s"Crime: $analysis")
        analysis.rows shouldBe 2000 +- 80
        r take 100 foreach println
        succeed
    }
  }

  it should "be ingested as a Table[Crime]" in {

    import CrimeParser._

    // Create the table
    val wsty: IO[Table[Crime]] = Table.parseResource(crimeFile, classOf[CrimeSpec])

    matchIO(wsty, Timeout(Span(10, Seconds))) {
      case t@HeadedTable(r, _) =>
        t.size shouldBe 4000
        r take 100 foreach println
        succeed
    }
  }
}
