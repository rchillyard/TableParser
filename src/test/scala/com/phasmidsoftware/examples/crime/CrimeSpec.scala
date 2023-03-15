package com.phasmidsoftware.examples.crime

import cats.effect.IO
import com.phasmidsoftware.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.table.{Analysis, HeadedTable, RawTable, Table}
import com.phasmidsoftware.util.EvaluateIO.matchIO
import com.phasmidsoftware.util.FP.resource
import com.phasmidsoftware.util.{FP, IOUsing}
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import scala.io.Source
import scala.util.Random

class CrimeSpec extends AnyFlatSpec with Matchers {

  behavior of "Crime"
  val crimeFile = "2023-01-metropolitan-street-sample.csv"

  it should "be ingested and analyzed as a RawTable" in {

    // Set up the source
    val sy: IO[Source] = IO.fromTry(for (u <- resource[CrimeSpec](crimeFile)) yield Source.fromURL(u))

    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser: RawTableParser = RawTableParser().setPredicate(TableParser.sampler(10))

    // Create the table
    val wsty: IO[RawTable] = parser.parse(sy)

    matchIO(wsty, Timeout(Span(10, Seconds))) {
      case t@HeadedTable(r, _) =>
        val analysis = Analysis(t)
        println(s"Crime: $analysis")
        analysis.rows shouldBe 400 +- 80
        r take 10 foreach println
        succeed
    }
  }

  it should "be ingested and written out in brief to CSV" in {
    import CrimeLocationRenderer._
    import CrimeParser._
    implicit val random: Random = new Random(0)
    val cti: IO[Table[Crime]] = IOUsing(Source.fromURL(classOf[Crime].getResource(crimeFile)))(x => Table.parseSource(x))

    val wi: IO[String] = for {
      ct <- cti
      lt <- IO(ct.mapOptional(m => m.brief))
      st <- IO(lt.filter(FP.sampler(10)))
      w <- st.toCSV
    } yield w

    matchIO(wi, Timeout(Span(20, Seconds))) {
      case w => w should startWith(
        """crimeID,longitude,latitude
          |8536e93fb3ce916daa4251bd53c1a4416ba4159a938340be4a7c40cd4873bfcf,-0.681541,50.792113
          |b2df387a964668ff00fbcfc3fb48cb71e42ac50de6e1ce90fbaf6ff373fbde51,0.134947,51.588063
          |627e9a2904b70e3ce9bd26c35bdb76a6e203adcbc0cbeeb99d0dca03fb3fe825,0.135579,51.584913
          |c0838e3ebf4956220e46f42748b0054b31fef1d89aa94356479fef0f5568cbd1,0.128436,51.583415
          |4387c65cd898b6295bdb034914d1c22d8dbbfe34f4e5e077f5742be434642640,0.12828,51.586251
          |5c137e8379542aa9df8c57c3bff5dc110236da87638258552adb3c43ca7a37b1,0.135611,51.582817
          |unidentified,0.141417,51.574915""".stripMargin)
    }
  }

}
