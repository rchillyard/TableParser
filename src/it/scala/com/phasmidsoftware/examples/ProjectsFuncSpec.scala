package com.phasmidsoftware.examples

import com.phasmidsoftware.parse.TableParser
import com.phasmidsoftware.render.{CsvGenerators, CsvRenderer, CsvRenderers}
import com.phasmidsoftware.table.Table.parse
import com.phasmidsoftware.table.{CsvGenerator, HeadedTable, Row, Table}
import com.phasmidsoftware.util.TryUsing
import java.io.File
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

    val filename = "TeamProject.csv"
    val mty: Try[Table[TeamProject]] = Table.parseResource(filename, classOf[ProjectsFuncSpec])
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    for (mt <- mty) {
      println(s"TeamProject: successfully read ${mt.size} rows")
      mt.size shouldBe 5
      mt foreach println
    }
  }

  it should "be ingested and written out to file using the given header" in {
    import TeamProjectParser._

    implicit val parser: TableParser[Table[TeamProject]] = implicitly[TableParser[Table[TeamProject]]]
    val mty: Try[Table[TeamProject]] = TryUsing(Source.fromURL(classOf[TeamProject].getResource("TeamProject.csv")))(parse(_))
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    for (mt <- mty) {
      import CsvGenerators._
      implicit val csvGenerator: CsvGenerator[TeamProject] = mt.maybeHeader match {
        case Some(h) => Row.csvGenerator(h)
        case None => createCsvGeneratorFromTeamProject(_.generator12(Grade))
      }
      import CsvRenderers._
      implicit val csvRenderer: CsvRenderer[TeamProject] = createCsvRendererForTeamProject(_.renderer12(Grade))
      mt.writeCSVFile(new File("TeamProjectOutput.csv"))
    }
  }

  it should "be ingested and written out properly using the given header" in {
    import TeamProjectParser._

    implicit val parser: TableParser[Table[TeamProject]] = implicitly[TableParser[Table[TeamProject]]]
    val mty: Try[Table[TeamProject]] = TryUsing(Source.fromURL(classOf[TeamProject].getResource("TeamProject.csv")))(parse(_))
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    for (mt <- mty) {
      import CsvGenerators._
      implicit val csvGenerator: CsvGenerator[TeamProject] = mt.maybeHeader match {
        case Some(h) => Row.csvGenerator(h)
        case None => createCsvGeneratorFromTeamProject(_.generator12(Grade))
      }
      import CsvRenderers._
      implicit val csvRenderer: CsvRenderer[TeamProject] = createCsvRendererForTeamProject(_.renderer12(Grade))
      mt.toCSV foreach println
    }
  }

  it should "be ingested and written out properly for team 1 using the given header" in {
    import TeamProjectParser._

    implicit val parser: TableParser[Table[TeamProject]] = implicitly[TableParser[Table[TeamProject]]]
    val mty: Try[Table[TeamProject]] = TryUsing(Source.fromURL(classOf[TeamProject].getResource("TeamProject.csv")))(parse(_))
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    for (mt <- mty) {
      import CsvGenerators._
      implicit val csvGenerator: CsvGenerator[TeamProject] = mt.maybeHeader match {
        case Some(h) => Row.csvGenerator(h)
        case None => createCsvGeneratorFromTeamProject(_.generator12(Grade))
      }
      import CsvRenderers._
      implicit val csvRenderer: CsvRenderer[TeamProject] = createCsvRendererForTeamProject(_.renderer12(Grade))
      mt.take(1).toCSV shouldBe
              """Team Number,Team Member 1,Team Member 2,Team Member 3,Team Member 4,Total Score,On Time,Scope Scale,Planning Presentation,Presentation,Idea,Use Cases,Acceptance Criteria,Team Execution,Code,Unit Tests,Repo,Remarks,Repository
                |,,,,,100,11,10,6,12,5,4,8,5,23,11,5,,
                |1,Leonhard Euler,Daniel Bernoulli,Isaac Newton,Srinivas Ramanujan,92.0,8.5,8.0,5.0,10.5,5.0,4.0,8.0,5.0,23.0,10.0,5.0,Presentation long and detailed.  Project excellent overall. Need to actually run UI myself.,https://github.com/youngbai/CSYE7200-MovieRecommendation
                |""".stripMargin
    }
  }

  it should "be ingested and written out properly for team 1 using the given header but skipping grades" in {
    import TeamProjectParser._

    implicit val parser: TableParser[Table[TeamProject]] = implicitly[TableParser[Table[TeamProject]]]
    val mty: Try[Table[TeamProject]] = TryUsing(Source.fromURL(classOf[TeamProject].getResource("TeamProject.csv")))(parse(_))
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    for (mt <- mty) {
      import CsvGenerators._
      implicit val csvGenerator: CsvGenerator[TeamProject] = mt.maybeHeader match {
        case Some(h) => Row.csvGenerator(h)
        case None => createCsvGeneratorFromTeamProject(_.generator12(Grade))
      }
      implicit val csvRenderer: CsvRenderer[TeamProject] = createCsvRendererForTeamProject(_.skipRenderer(12))
      mt.take(1).toCSV shouldBe
              """Team Number,Team Member 1,Team Member 2,Team Member 3,Team Member 4,Total Score,On Time,Scope Scale,Planning Presentation,Presentation,Idea,Use Cases,Acceptance Criteria,Team Execution,Code,Unit Tests,Repo,Remarks,Repository
                |,,,,,100,11,10,6,12,5,4,8,5,23,11,5,,
                |1,Leonhard Euler,Daniel Bernoulli,Isaac Newton,Srinivas Ramanujan,,,,,,,,,,,,,Presentation long and detailed.  Project excellent overall. Need to actually run UI myself.,https://github.com/youngbai/CSYE7200-MovieRecommendation
                |""".stripMargin
    }
  }

  it should "be ingested and written out properly using fabricated header" in {
    import TeamProjectParser._

    implicit val parser: TableParser[Table[TeamProject]] = implicitly[TableParser[Table[TeamProject]]]
    val mty: Try[Table[TeamProject]] = TryUsing(Source.fromURL(classOf[TeamProject].getResource("TeamProject.csv")))(parse(_))
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    for (mt <- mty) {
      import CsvGenerators._
      implicit val csvGenerator: CsvGenerator[TeamProject] = mt.maybeHeader match {
        case _ => createCsvGeneratorFromTeamProject(_.generator12(Grade))
      }
      import CsvRenderers._
      implicit val csvRenderer: CsvRenderer[TeamProject] = createCsvRendererForTeamProject(_.renderer12(Grade))
      mt.toCSV shouldBe
              """team.number,team.member_1,team.member_2,team.member_3,team.member_4,grade.totalScore,grade.onTime,grade.scopeScale,grade.planningPresentation,grade.presentation,grade.idea,grade.useCases,grade.acceptanceCriteria,grade.teamExecution,grade.code,grade.unitTests,grade.repo,remarks,repository
                |1,Leonhard Euler,Daniel Bernoulli,Isaac Newton,Srinivas Ramanujan,92.0,8.5,8.0,5.0,10.5,5.0,4.0,8.0,5.0,23.0,10.0,5.0,Presentation long and detailed.  Project excellent overall. Need to actually run UI myself.,https://github.com/youngbai/CSYE7200-MovieRecommendation
                |2,Ringo Starr,George Harrison,Paul McCartney,,83.5,8.5,10.0,5.0,9.0,5.0,4.0,6.0,3.0,22.0,7.0,4.0,Presentation quite confused. Part of project not achieved. ,https://github.com/PhoenixMay/MovieRecommendation
                |3,Pablo Picasso,Joan Miro,,,92.0,11.0,10.0,5.0,10.0,5.0,4.0,7.0,4.0,23.0,9.0,4.0,Presentation OK. What did they do about these challenges.,https://github.com/jianghanw/CSYE7200_Team_Project_Team3
                |4,Ludwig van Beethoven,Wolfgang Amadeus Mozart,,,88.5,11.0,10.0,6.0,10.5,5.0,3.5,6.0,4.0,21.5,7.0,4.0,A/C not clearly defined (planning pres?),https://github.com/Essexwwz/heart-health-indicator
                |5,Winston Churchill,Clement Attlee,Harold McMillan,,88.5,11.0,9.5,5.0,11.0,5.0,4.0,7.0,2.0,22.0,7.0,5.0,Presentation long and detailed.  Project excellent overall. A/C not shown clearly.,https://github.com/CSYE7200-21FALL-TEAM6
                |""".stripMargin
    }
  }

  it should "be ingested and written out properly for team 1 using fabricated header" in {
    import TeamProjectParser._

    implicit val parser: TableParser[Table[TeamProject]] = implicitly[TableParser[Table[TeamProject]]]
    val mty: Try[Table[TeamProject]] = TryUsing(Source.fromURL(classOf[TeamProject].getResource("TeamProject.csv")))(parse(_))
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    for (mt <- mty) {
      import CsvGenerators._
      implicit val csvGenerator: CsvGenerator[TeamProject] = mt.maybeHeader match {
        case _ => createCsvGeneratorFromTeamProject(_.generator12(Grade))
      }
      import CsvRenderers._
      implicit val csvRenderer: CsvRenderer[TeamProject] = createCsvRendererForTeamProject(_.renderer12(Grade))
      mt.take(1).toCSV shouldBe
              """team.number,team.member_1,team.member_2,team.member_3,team.member_4,grade.totalScore,grade.onTime,grade.scopeScale,grade.planningPresentation,grade.presentation,grade.idea,grade.useCases,grade.acceptanceCriteria,grade.teamExecution,grade.code,grade.unitTests,grade.repo,remarks,repository
                |1,Leonhard Euler,Daniel Bernoulli,Isaac Newton,Srinivas Ramanujan,92.0,8.5,8.0,5.0,10.5,5.0,4.0,8.0,5.0,23.0,10.0,5.0,Presentation long and detailed.  Project excellent overall. Need to actually run UI myself.,https://github.com/youngbai/CSYE7200-MovieRecommendation
                |""".stripMargin
    }
  }

  it should "be ingested and written out properly for team 1 using fabricated header but skipping grades" in {
    import TeamProjectParser._

    implicit val parser: TableParser[Table[TeamProject]] = implicitly[TableParser[Table[TeamProject]]]
    val mty: Try[Table[TeamProject]] = TryUsing(Source.fromURL(classOf[TeamProject].getResource("TeamProject.csv")))(parse(_))
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    for (mt <- mty) {
      implicit val csvGenerator: CsvGenerator[TeamProject] = mt.maybeHeader match {
        case _ => createCsvGeneratorFromTeamProject(_.skipGenerator)
      }
      implicit val csvRenderer: CsvRenderer[TeamProject] = createCsvRendererForTeamProject(_.skipRenderer())
      mt.take(1).toCSV shouldBe
              """team.number,team.member_1,team.member_2,team.member_3,team.member_4,,remarks,repository
                |1,Leonhard Euler,Daniel Bernoulli,Isaac Newton,Srinivas Ramanujan,,Presentation long and detailed.  Project excellent overall. Need to actually run UI myself.,https://github.com/youngbai/CSYE7200-MovieRecommendation
                |""".stripMargin
    }
  }

  private def createCsvGeneratorFromTeamProject(function: CsvGenerators => CsvGenerator[Grade]): CsvGenerator[TeamProject] = {
    val csvGenerators = new CsvGenerators {}
    import CsvGenerators._
    implicit val optionStringGenerator: CsvGenerator[Option[String]] = csvGenerators.optionGenerator[String]
    implicit val teamGenerator: CsvGenerator[Team] = csvGenerators.generator5(Team)
    implicit val gradeGenerator: CsvGenerator[Grade] = function(csvGenerators)
    csvGenerators.generator4(TeamProject)
  }

  private def createCsvRendererForTeamProject(function: CsvRenderers => CsvRenderer[Grade]): CsvRenderer[TeamProject] = {
    val csvRenderers = new CsvRenderers {}
    import CsvRenderers._
    implicit val optionStringGenerator: CsvRenderer[Option[String]] = csvRenderers.optionRenderer[String]
    implicit val teamGenerator: CsvRenderer[Team] = csvRenderers.renderer5(Team)
    implicit val gradeGenerator: CsvRenderer[Grade] = function(csvRenderers)
    csvRenderers.renderer4(TeamProject)
  }

}
