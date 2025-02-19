package com.phasmidsoftware.tableparser.cats.examples.crime

import cats.effect.IO
import com.phasmidsoftware.tableparser.core.examples.crime.Crime
import com.phasmidsoftware.tableparser.core.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.tableparser.core.table.{Analysis, HeadedTable, RawTable, Table}
import com.phasmidsoftware.tableparser.core.util.EvaluateIO.matchIO
import com.phasmidsoftware.tableparser.core.util.FP.resourceForClass
import com.phasmidsoftware.tableparser.core.util.{FP, IOUsing}
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import scala.io.Source
import scala.util.{Random, Try}

class CrimeSpec extends AnyFlatSpec with Matchers {

  behavior of "Crime"
  val crimeFile = "2023-01-metropolitan-street-sample.csv"

  it should "be ingested and analyzed as a RawTable" in {

    // Set up the source
    val sy: Try[Source] = for (u <- resourceForClass(crimeFile, classOf[Crime])) yield Source.fromURL(u)

    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser: RawTableParser = RawTableParser().setPredicate(TableParser.sampler(10))

    // Create the table
    val wsty: IO[RawTable] = IO.fromTry(parser.parse(sy))

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
    import com.phasmidsoftware.tableparser.core.examples.crime.CrimeLocationRenderer._
    import com.phasmidsoftware.tableparser.core.examples.crime.CrimeParser._
    implicit val random: Random = new Random(0)
    val cti: IO[Table[Crime]] = IOUsing(Source.fromURL(classOf[Crime].getResource(crimeFile)))(x => IO.fromTry(Table.parseSource(x)))

    val wi: IO[String] = for {
      ct <- cti
      lt <- IO(ct.mapOptional(m => m.brief))
      st <- IO(lt.filter(FP.sampler(10)))
      w <- IO.fromTry(st.toCSV)
    } yield w

    matchIO(wi, Timeout(Span(20, Seconds))) {
      case w =>
        w should startWith(
          """crimeID,longitude,latitude
            |""".stripMargin)
    }
  }

}
