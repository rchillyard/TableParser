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

  /**
   * The following file is ignored for git purposes:
   * You need to download and extract it from here:
   * [[https://www.kaggle.com/datasets/marshuu/crimes-in-uk-2023/download]]
   */
  val crimeFile = "2023-01-metropolitan-street.csv"

  it should "be ingested and analyzed as a RawTable" in {

    // Set up the source
    val sy: IO[Source] = IO.fromTry(for (u <- resource[CrimeFuncSpec](crimeFile)) yield Source.fromURL(u))

    val fraction = 1
    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser: RawTableParser = RawTableParser().setPredicate(TableParser.sampler(fraction))

    // Create the table
    val wsty: IO[RawTable] = parser.parse(sy)

    // CONSIDER how is it that this test runs in around 157 seconds yet the timeout is set to 30 seconds?
    matchIO(wsty, Timeout(Span(30, Seconds))) {
      case t@HeadedTable(r, _) =>
        val analysis = Analysis(t)
        println(s"Crime: $analysis")
        analysis.rows shouldBe 87211/fraction +- 2000
        r take 10 foreach println
        succeed
    }
  }

  it should "be ingested as a Table[Crime]" in {

    import CrimeParser._

    // Create the table
    val wsty: IO[Table[Crime]] = Table.parseResource(crimeFile, classOf[CrimeFuncSpec])

    matchIO(wsty, Timeout(Span(60, Seconds))) {
      case t@HeadedTable(r, _) =>
        t.size shouldBe 87211
        r take 100 foreach println
        succeed
    }
  }

  it should "be ingested and written out properly to CSV" in {
    import CrimeParser._
    import CrimeRenderer._

    val mti: IO[Table[Crime]] = IOUsing(Source.fromURL(classOf[Crime].getResource(crimeFile)))(x => Table.parseSource(x))

    val wi: IO[String] = mti flatMap (_.toCSV)
    matchIO(wi, Timeout(Span(60, Seconds))) {
      case w => w should startWith("crimeID,month,reportedBy,fallsWithin,longitude,latitude,location,lsoaCode,lsoaName,crimeType,lastOutcomeCategory,context\n8536e93fb3ce916daa4251bd53c1a4416ba4159a938340be4a7c40cd4873bfcf,2023-01,Metropolitan Police Service,Metropolitan Police Service,-0.681541,50.792113,On or near Fletcher Way,E01031444,Arun 016B,Violence and sexual offences,Under investigation,")
    }
  }

  it should "be ingested and written out in brief to CSV" in {
    import CrimeLocationRenderer._
    import CrimeParser._

    val cti: IO[Table[Crime]] = IOUsing(Source.fromURL(classOf[Crime].getResource(crimeFile)))(x => Table.parseSource(x))

    val wi: IO[String] = for (ct <- cti; lt <- IO(ct.mapOptional(m => m.brief)); _ = println(s"rows: ${lt.size}"); w <- lt.toCSV) yield w

    matchIO(wi, Timeout(Span(60, Seconds))) {
      case w => w should startWith("crimeID,longitude,latitude\n8536e93fb3ce916daa4251bd53c1a4416ba4159a938340be4a7c40cd4873bfcf,-0.681541,50.792113\n483d52d514591a895c829dece6091c31f797b7dcfd0735ac89685d1d4dabf899,-0.684107,50.780541\n63343c1f1236bad8ce08d130f37760172dc33b20af2b56fafd9189001d014c39,-0.928552,51.923331\na3d980f554d3ece9e8dcda8518ae87bfa9c75d62396105d63fd10390eb7879ed,-0.772051,51.827897\nbfb1d1da32341b7129e789130001d96f7e603088593dc55e30294bc01670ff9e,-0.804965,51.811332\nde18f4ebeefb1d66f3be2c34f1fc056d751d763b57b86c28955ec793d0f77867,0.724588,52.034478\nunidentified,0.140127,51.588913")
    }
  }

}
