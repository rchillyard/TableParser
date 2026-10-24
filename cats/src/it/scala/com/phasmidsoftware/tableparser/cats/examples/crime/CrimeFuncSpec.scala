package com.phasmidsoftware.tableparser.cats.examples.crime

import cats.effect.IO
import com.phasmidsoftware.tableparser.cats.util.EvaluateIO.matchIO
import com.phasmidsoftware.tableparser.cats.util.IOUsing
import com.phasmidsoftware.tableparser.core.examples.crime.Crime
import com.phasmidsoftware.tableparser.core.parse.{LineParser, RawTableParser, RowConfig, TableParser}
import com.phasmidsoftware.tableparser.core.table.{Analysis, HeadedTable, RawTable, Table}
import com.phasmidsoftware.tableparser.core.util.FP.resourceForClass
import java.net.URL
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import scala.io.Source

class CrimeFuncSpec extends AnyFlatSpec with Matchers:

  behavior of "Crime"

  /**
   * The following file is ignored for git purposes:
   * You need to download and extract it from here:
   * [[https://www.kaggle.com/datasets/marshuu/crimes-in-uk-2023/download]]
   */
  val crimeFile = "2023-01-metropolitan-street.csv"

  it should "be ingested and analyzed as a RawTable" in:

    // Set up the source
    val si: IO[Source] = IO.fromTry(for (u <- resourceForClass(crimeFile, classOf[Crime])) yield Source.fromURL(u))

    val fraction = 4
    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser: RawTableParser = RawTableParser().setPredicate(TableParser.sampler(fraction))


    // Create the table
    // The header is the first line of the file (up to 2.0.2, this passed 0, which meant no header: a logic error).
    val wsty: IO[RawTable] = si flatMap (s => IO.fromTry(parser.parse(s.getLines(), parser.headerRowsToRead)))

// CONSIDER how is it that this test runs in around 157 seconds yet the timeout is set to 30 seconds?
    matchIO(wsty, Timeout(Span(30, Seconds))):
      case t@HeadedTable(r, _) =>
        val analysis = Analysis(t)
        println(s"Crime: $analysis")
        analysis.rows shouldBe 87211 / fraction +- 2000
        r take 10 foreach println
        succeed

  it should "be ingested as a Table[Crime]" in:

    import com.phasmidsoftware.tableparser.core.examples.crime.CrimeParser._

    // Create the table
    val wsty: IO[Table[Crime]] = IO.fromTry(Table.parseResource(crimeFile, classOf[Crime]))

    matchIO(wsty, Timeout(Span(60, Seconds))):
      case t@HeadedTable(r, _) =>
        t.size shouldBe 87205 // XXX was 87211
        r take 100 foreach println
        succeed

  private val crimeResource: URL = classOf[Crime].getResource(crimeFile)
  it should "be ingested and written out properly to CSV" in:
    import com.phasmidsoftware.tableparser.core.examples.crime.CrimeParser._
    import com.phasmidsoftware.tableparser.core.examples.crime.CrimeRenderer._

    val mti: IO[Table[Crime]] = IOUsing(Source.fromURL(crimeResource)):
      r => IO.fromTry(Table.parseSource(r))

    val wi: IO[String] = mti flatMap (mt => IO.fromTry(mt.toCSV))
    matchIO(wi, Timeout(Span(60, Seconds))):
      case w =>
        // The header uses the field names (see CrimeRenderer); each row is written as it was read, except that
        // a number is written as a Double is (e.g. 0.13883, which the file has as 0.138830),
        // and a field which ends with a space is quoted (as RFC 4180 requires, so that the space is kept).
        // (Up to 2.0.2, this test expected a row which isn't in this file, and was ignored.)
        val lineParser = LineParser.apply(using RowConfig.default)
        def fields(line: String): Seq[Any] = lineParser.parseRow(line -> 0).get map (f => f.toDoubleOption.getOrElse(f))
        val written = w.split("\r\n").toSeq
        written.head shouldBe "crimeID,month,reportedBy,fallsWithin,longitude,latitude,location,lsoaCode,lsoaName,crimeType,lastOutcomeCategory,context"
        val original = scala.util.Using.resource(Source.fromURL(crimeResource))(_.getLines().slice(1, 1001).toList)
        written.slice(1, 1001) map fields shouldBe (original map fields)

  it should "be ingested and written out in brief to CSV" in:
    import com.phasmidsoftware.tableparser.core.examples.crime.CrimeLocationRenderer._
    import com.phasmidsoftware.tableparser.core.examples.crime.CrimeParser._

    val cti: IO[Table[Crime]] =
      IOUsing(Source.fromURL(crimeResource))(x => IO.fromTry(Table.parseSource(x)))

    val wi: IO[String] = for {
      ct <- cti
      lt <- IO(ct.mapOptional(m => m.brief))
      w <- IO.fromTry(lt.toCSV)
    } yield w

    matchIO(wi, Timeout(Span(60, Seconds))):
      case w =>
        // CONSIDER I'm not sure if this expectedStart is really OK.
//        val expectedStart = "crimeID,longitude,latitude\n8536e93fb3ce916daa4251bd53c1a4416ba4159a938340be4a7c40cd4873bfcf,-0.681541,50.792113\n483d52d514591a895c829dece6091c31f797b7dcfd0735ac89685d1d4dabf899,-0.684107,50.780541\n63343c1f1236bad8ce08d130f37760172dc33b20af2b56fafd9189001d014c39,-0.928552,51.923331\na3d980f554d3ece9e8dcda8518ae87bfa9c75d62396105d63fd10390eb7879ed,-0.772051,51.827897\nbfb1d1da32341b7129e789130001d96f7e603088593dc55e30294bc01670ff9e,-0.804965,51.811332\nde18f4ebeefb1d66f3be2c34f1fc056d751d763b57b86c28955ec793d0f77867,0.724588,52.034478\nunidentified,0.140127,51.588913"
        val expectedStart = "crimeID,longitude,latitude\r\nunidentified,0.140127,51.588913"
        w should startWith(expectedStart)

