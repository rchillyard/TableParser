package com.phasmidsoftware.examples

import com.phasmidsoftware.parse.TableParser
import com.phasmidsoftware.table.Table.parse
import com.phasmidsoftware.table.{HeadedTable, Table}
import com.phasmidsoftware.util.TryUsing
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source
import scala.util._

class ProjectsFuncSpec extends AnyFlatSpec with Matchers {

  behavior of "TeamProject table"

  /**
    * NOTE: it is perfectly proper for there to be a number of parsing problems.
    * These are application-specific and are not indicative of any bugs in the
    * TableParser library itself.
    */
  it should "be ingested properly" in {
    import TeamProjectParser._

    //    val filename = "CSYE7200-FALL2021-Project.csv"
    val filename = "TeamProject.csv"
    //    implicit val codec = Codec("utf-16le")
    val mty: Try[Table[TeamProject]] = Table.parseResource(filename, classOf[ProjectsFuncSpec])
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    for (mt <- mty) {
      println(s"TeamProject: successfully read ${mt.size} rows")
      mt.size shouldBe 5
      mt foreach println
    }
  }

  /**
    * NOTE: it is perfectly proper for there to be a number of parsing problems.
    * These are application-specific and are not indicative of any bugs in the
    * TableParser library itself.
    */
  it should "be ingested and written out properly" in {
    import TeamProjectParser._

    implicit val parser: TableParser[Table[TeamProject]] = implicitly[TableParser[Table[TeamProject]]]
    val mty: Try[Table[TeamProject]] = TryUsing(Source.fromURL(classOf[TeamProject].getResource("CSYE7200-FALL2021-Project.csv")))(parse(_))
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    for (mt <- mty) {
      // TODO implement this part
      //      mt.generateCsvHeader
      //      mt.toCSV
    }
  }

}
