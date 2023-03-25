package com.phasmidsoftware.examples.crime

import cats.effect.IO
import com.phasmidsoftware.parse.{RawTableParser, StandardStringsParser, TableParser}
import com.phasmidsoftware.table._
import com.phasmidsoftware.util.EvaluateIO.matchIO
import com.phasmidsoftware.util.FP.resource
import com.phasmidsoftware.util.{FP, IOUsing}
import java.net.URL
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import scala.io.Source
import scala.util.{Random, Success, Try}

class CrimeSpec extends AnyFlatSpec with Matchers {

  behavior of "CrimeLocation"

  it should "parse from Strings" in {
    val header: Header = Header.create("longitude", "latitude", "location", "LSOA code", "LSOA name")
    val parser = StandardStringsParser[CrimeLocation]()
    val location: Try[CrimeLocation] = parser.parse((Seq("0.140127", "51.588913", "On or near Beansland Grove", "E01000027", "Barking and Dagenham 001A"), 0))(header)
    location shouldBe Success(CrimeLocation(0.140127, 51.588913, "On or near Beansland Grove", "E01000027", "Barking and Dagenham 001A"))
  }

  behavior of "Crime"
  val crimeSampleFile = "2023-01-metropolitan-street-sample.csv"
  val triedCrimeSampleResource: Try[URL] = resource[CrimeSpec](crimeSampleFile)

  it should "be ingested and analyzed as a RawTable" in {

    // Set up the source
    val sy: IO[Source] = IO.fromTry(for (u <- triedCrimeSampleResource) yield Source.fromURL(u))

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

  it should "get the order right for Crime" in {
    val sequence1 = Sequence(1)
    val sequence2 = sequence1.next
    val x1 = Crime(sequence1, None, "", "", "", None, "", "", "")
    val x2 = Crime(sequence2, None, "", "", "", None, "", "", "")
    val co = implicitly[Ordering[Crime]]
    co.compare(x1, x2) shouldBe -1
  }

  it should "get the order right for CrimeBrief" in {
    val x1 = CrimeBrief(BigInt(0), 0.0, 0.0)
    val x2 = CrimeBrief(BigInt(1), 0.0, 0.0)
    val co = implicitly[Ordering[CrimeBrief]]
    co.compare(x1, x2) shouldBe -1
    co.compare(x2, x1) shouldBe 1
  }

  it should "be ingested and written out in brief to CSV" in {
    import CrimeParser._
    implicit val random: Random = new Random(0)
    val wi: IO[String] = for {
      url <- IO.fromTry(Crime.crimeTriedResource)
      ct <- IOUsing(Source.fromURL(url))(x => Table.parseSource(x))
      lt <- IO(ct.mapOptional(m => m.brief))
      st <- IO(lt.filter(FP.sampler(10)))
      w <- st.toCSV
    } yield w

    matchIO(wi, Timeout(Span(20, Seconds))) {
      case w =>
        // NOTE that the output from a parallel store is random (why?).
        w should startWith("""crimeID,longitude,latitude""".stripMargin)
    }
  }

  it should "doMain" in {
    implicit val random: Random = new Random(0)
    matchIO(Crime.doMain(Crime.crimeTriedResource), Timeout(Span(20, Seconds))) {
      case w => w.lines().count() shouldBe 156
    }
  }
}
