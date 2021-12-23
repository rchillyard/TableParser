package com.phasmidsoftware.table

import com.phasmidsoftware.parse.TableParser
import com.phasmidsoftware.table.Table.parse
import com.phasmidsoftware.util.TryUsing
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source
import scala.util._

class MovieFuncSpec extends AnyFlatSpec with Matchers {

  behavior of "Movie table"

  /**
    * NOTE: it is perfectly proper for there to be a number of parsing problems.
    * These are application-specific and are not indicative of any bugs in the
    * TableParser library itself.
    */
  it should "be ingested properly" in {
    import MovieParser._

    val mty: Try[Table[Movie]] = Table.parseResource("movie_metadata.csv")
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    for (mt <- mty) {
      println(s"Movie: successfully read ${mt.size} rows")
      mt.size shouldBe 1567
      mt take 10 foreach println
    }
  }

  /**
   * NOTE: it is perfectly proper for there to be a number of parsing problems.
   * These are application-specific and are not indicative of any bugs in the
   * TableParser library itself.
   */
  it should "be ingested and written out properly" in {
    import MovieParser._

    implicit val parser: TableParser[Table[Movie]] = implicitly[TableParser[Table[Movie]]]
    val mty: Try[Table[Movie]] = TryUsing(Source.fromURL(classOf[Movie].getResource("movie_metadata.csv")))(parse(_))
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    for (mt <- mty) {
      // TODO implement this part
//      mt.generateCsvHeader
//      mt.toCSV
    }
  }

}
