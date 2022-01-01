package com.phasmidsoftware.table

import com.phasmidsoftware.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.util.FP.resource
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source
import scala.util._

class AirBNBSpec extends AnyFlatSpec with Matchers {

  behavior of "AirBNB table"

  /**
   * NOTE: it is perfectly proper for there to be a number of parsing problems.
   * These are application-specific and are not indicative of any bugs in the
   * TableParser library itself.
   */
  it should "be ingested properly" in {
    val airBNBFile = "/airbnb2.csv"

    // Set up the source
    val sy: Try[Source] = for (u <- resource[AirBNBSpec](airBNBFile)) yield Source.fromURL(u)

    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser = RawTableParser().setPredicate(TableParser.sampler(2)).setMultiline(true)

    // Create the table
    val wsty: Try[Table[Seq[String]]] = parser parse sy

    wsty should matchPattern { case Success(HeadedTable(_, _)) => }
    wsty match {
      case Success(t@HeadedTable(r, _)) =>
        val analysis = Analysis(t)
        println(s"AirBNB: $analysis")
        analysis.rows shouldBe 127 +- 32
        r take 100 foreach println
    }
  }
}
