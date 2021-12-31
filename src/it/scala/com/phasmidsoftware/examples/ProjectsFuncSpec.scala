package com.phasmidsoftware.examples

import com.phasmidsoftware.parse.TableParser
import com.phasmidsoftware.render.{CsvGenerators, CsvRenderer, CsvRenderers}
import com.phasmidsoftware.table.Table.parse
import com.phasmidsoftware.table.{CsvGenerator, HeadedTable, Row, Table}
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

  it should "be ingested and written out properly" in {
    import TeamProjectParser._

    implicit val parser: TableParser[Table[TeamProject]] = implicitly[TableParser[Table[TeamProject]]]
    val mty: Try[Table[TeamProject]] = TryUsing(Source.fromURL(classOf[TeamProject].getResource("TeamProject.csv")))(parse(_))
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    for (mt <- mty) {
      implicit val csvGenerator: CsvGenerator[TeamProject] = mt.maybeHeader match {
        case Some(h) => Row.csvGenerator(h)
        case None => createCsvGeneratorFromTeamProject
      }
      implicit val csvRenderer: CsvRenderer[TeamProject] = createCsvRendererForTeamProject
      mt.toCSV foreach println
    }
  }

  private def createCsvGeneratorFromTeamProject: CsvGenerator[TeamProject] = {
    val csvGenerators = new CsvGenerators {}
    import CsvGenerators._
    implicit val optionStringGenerator: CsvGenerator[Option[String]] = csvGenerators.optionGenerator[String]
    implicit val teamGenerator: CsvGenerator[Team] = csvGenerators.generator5(Team)
    implicit val gradeGenerator: CsvGenerator[Grade] = csvGenerators.generator12(Grade)
    csvGenerators.generator4(TeamProject)
  }

  private def createCsvRendererForTeamProject: CsvRenderer[TeamProject] = {
    val csvRenderers = new CsvRenderers {}
    import CsvRenderers._
    implicit val optionStringGenerator: CsvRenderer[Option[String]] = csvRenderers.optionRenderer[String]
    implicit val teamGenerator: CsvRenderer[Team] = csvRenderers.renderer5(Team)
    implicit val gradeGenerator: CsvRenderer[Grade] = csvRenderers.renderer12(Grade)
    csvRenderers.renderer4(TeamProject)
  }

}
