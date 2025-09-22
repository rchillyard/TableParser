package com.phasmidsoftware.tableparser.zio.examples.crime

import com.phasmidsoftware.tableparser.core.examples.crime.Crime
import com.phasmidsoftware.tableparser.core.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.tableparser.core.table.{Analysis, HeadedTable, RawTable}
import com.phasmidsoftware.tableparser.core.util.FP.resourceForClass
import com.phasmidsoftware.tableparser.zio.util.EvaluateZIO
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source
import scala.util.Try
import zio._

class CrimeSpec extends AnyFlatSpec with Matchers {

  behavior of "Crime"
  val crimeFile = "2023-01-metropolitan-street-sample.csv"

  it should "be ingested and analyzed as a RawTable" in {

    // Set up the source
    val sy: Try[Source] = for (u <- resourceForClass(crimeFile, classOf[Crime])) yield Source.fromURL(u)

    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser: RawTableParser = RawTableParser().setPredicate(TableParser.sampler(10))

    // Create the table
    val wsty: Task[RawTable] = ZIO.fromTry(parser.parse(sy))

    EvaluateZIO.matchIO(wsty) {
      case t@HeadedTable(r, _) =>
        val analysis = Analysis(t)
        println(s"Crime: $analysis")
        analysis.rows shouldBe 400 +- 80
        r take 10 foreach println
        true
    }
  }

//  it should "be ingested and written out in brief to CSV" in {
//    implicit val random: Random = new Random(0)
//    val cti: IO[Table[Crime]] = IOUsing(Source.fromURL(classOf[Crime].getResource(crimeFile)))(x => IO.fromTry(Table.parseSource(x)))
//
//    val wi: IO[String] = for {
//      ct <- cti
//      lt <- IO(ct.mapOptional(m => m.brief))
//      st <- IO(lt.filter(FP.sampler(10)))
//      w <- IO.fromTry(st.toCSV)
//    } yield w
//
//    matchIO(wi, Timeout(Span(20, Seconds))) {
//      case w =>
//        w should startWith(
//          """crimeID,longitude,latitude
//            |""".stripMargin)
//    }
//  }

}
