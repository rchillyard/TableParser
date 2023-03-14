package com.phasmidsoftware.examples.crime

import cats.effect.IO
import com.phasmidsoftware.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.table.{Analysis, HeadedTable, RawTable, Table}
import com.phasmidsoftware.util.EvaluateIO.matchIO
import com.phasmidsoftware.util.FP.resource
import com.phasmidsoftware.util.IOUsing
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import scala.io.Source

class CrimeFuncSpec extends AnyFlatSpec with Matchers {

  behavior of "Crime"
  val crimeFile = "2023-01-metropolitan-street.csv"

  it should "be ingested and analyzed as a RawTable" in {

    // Set up the source
    val sy: IO[Source] = IO.fromTry(for (u <- resource[CrimeFuncSpec](crimeFile)) yield Source.fromURL(u))

    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser: RawTableParser = RawTableParser().setPredicate(TableParser.sampler(2))

    // Create the table
    val wsty: IO[RawTable] = parser.parse(sy)

    matchIO(wsty, Timeout(Span(10, Seconds))) {
      case t@HeadedTable(r, _) =>
        val analysis = Analysis(t)
        println(s"Crime: $analysis")
        analysis.rows shouldBe 2000 +- 80
        r take 10 foreach println
        succeed
    }
  }

  it should "be ingested as a Table[Crime]" in {

    import CrimeParser._

    // Create the table
    val wsty: IO[Table[Crime]] = Table.parseResource(crimeFile, classOf[CrimeFuncSpec])

    matchIO(wsty, Timeout(Span(10, Seconds))) {
      case t@HeadedTable(r, _) =>
        t.size shouldBe 4000
        r take 100 foreach println
        succeed
    }
  }

  it should "be ingested and written out properly to CSV" in {
    import CrimeParser._
    import CrimeRenderer._

    val mti: IO[Table[Crime]] = IOUsing(Source.fromURL(classOf[Crime].getResource(crimeFile)))(x => Table.parseSource(x))

    val wi: IO[String] = mti flatMap (_.toCSV)
    matchIO(wi, Timeout(Span(20, Seconds))) {
      case w => w should startWith("crimeID,month,reportedBy,fallsWithin,longitude,latitude,location,lsoaCode,lsoaName,crimeType,lastOutcomeCategory,context\n8536e93fb3ce916daa4251bd53c1a4416ba4159a938340be4a7c40cd4873bfcf,2023-01,Metropolitan Police Service,Metropolitan Police Service,-0.681541,50.792113,On or near Fletcher Way,E01031444,Arun 016B,Violence and sexual offences,Under investigation,")
    }
  }
}
