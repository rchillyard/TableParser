/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.examples

import com.phasmidsoftware.parse._
import com.phasmidsoftware.table.{HeadedTable, Header, Table}
import java.net.URL

/**
 * This class represents a TeamProject from the IMDB data file on Kaggle.
 * Although the limitation on 22 fields in a case class has partially gone away, it's still convenient to group the different attributes together into logical classes.
 *
 * Created by scalaprof on 9/12/16.
 *
 * Common questions in this assignment:
 * 1. Where is main method?
 * In most case, you don't need to run main method for assignments.
 * Unit tests are provided to test your implementation.
 * In this assignment, you will find the `object TeamProject extends App`,
 * the `App` trait can be used to quickly turn objects into executable programs.
 * You can read the official doc of Scala for more details.
 *
 * 2. How to understand the whole program in this assignment?
 * I won't suggest you to understand the whole program in this assignment,
 * there are some advanced features like `implicit` which hasn't been covered in class.
 * You should be able to understand it before midterm.
 * I will suggest you only focus on each TO BE IMPLEMENTED in the assignments.
 *
 */
case class TeamProject(team: Team, grade: Grade, remarks: String, repository: URL)

case class Team(number: Int, member_1: String, member_2: Option[String], member_3: Option[String], member_4: Option[String])

case class Grade(totalScore: Double, onTime: Double, scopeScale: Double, planningPresentation: Double, presentation: Double, idea: Double, useCases: Double, acceptanceCriteria: Double, teamExecution: Double, code: Double, unitTests: Double, repo: Double)

object TeamProjectParser extends CellParsers {

  def camelToSnakeCaseColumnNameMapper(w: String): String = w.replaceAll("([A-Z\\d])", "_$1")

  def identifierToCapitalizedWordColumnNameMapper(w: String): String = w.replaceAll("([A-Z\\d])", " $1").replaceAll("_", "")

  implicit val teamProjectColumnHelper: ColumnHelper[TeamProject] = columnHelper(camelToSnakeCaseColumnNameMapper _)
  implicit val gradeColumnHelper: ColumnHelper[Grade] = columnHelper(identifierToCapitalizedWordColumnNameMapper _)
  implicit val teamColumnHelper: ColumnHelper[Team] = columnHelper(identifierToCapitalizedWordColumnNameMapper _, Some("$x $c"))
  implicit val optionalStringParser: CellParser[Option[String]] = cellParserOption
  implicit val teamParser: CellParser[Team] = cellParser5(Team)
  implicit val gradeParser: CellParser[Grade] = cellParser12(Grade)
  implicit val attributesParser: CellParser[AttributeSet] = cellParser(AttributeSet.apply: String => AttributeSet)
  implicit val teamProjectParser: CellParser[TeamProject] = cellParser4(TeamProject)

  implicit object TeamProjectConfig extends DefaultRowConfig {
    override val listEnclosure: String = ""
  }

  val parser: StandardRowParser[TeamProject] = StandardRowParser.create[TeamProject]
}

sealed trait TeamProjectTableParser extends StringTableParser[Table[TeamProject]] {
  type Row = TeamProject

  val maybeFixedHeader: Option[Header] = None

  val headerRowsToRead: Int = 2

  override val forgiving: Boolean = true

  val rowParser: RowParser[Row, String] = TeamProjectParser.parser

  protected def builder(rows: Iterable[TeamProject], header: Header): Table[Row] = HeadedTable(rows, header)
}

object TeamProjectTableParser extends TeamProjectTableParser
