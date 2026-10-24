package com.phasmidsoftware.tableparser.core.examples

import com.phasmidsoftware.tableparser.core.render.{CsvProduct, CsvProductGenerator}
import com.phasmidsoftware.tableparser.core.table.{HeadedTable, Table}
import com.phasmidsoftware.tableparser.core.util.EvaluateTry.matchTry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source
import scala.util.Using

class UnstructuredMovieSpec extends AnyFlatSpec with Matchers:

  import UnstructuredMovie._

  behavior of "UnstructuredMovie"

  private val (header, avatar) = Using.resource(Source.fromURL(classOf[UnstructuredMovie].getResource("movie_metadata.csv"))) { s =>
    val lines = s.getLines()
    (lines.next(), lines.next())
  }

  it should "parse all 28 columns of the movie database into one case class" in:
    header.split(",").length shouldBe 28
    matchTry(Table.parseResource[Table[UnstructuredMovie]]("movie_metadata.csv", classOf[UnstructuredMovie])):
      case mt@HeadedTable(_, _) =>
        // Every row parses (Movie, with its stricter types, gets 1567).
        mt.size shouldBe 1609
        mt.find(_.movieTitle.startsWith("Avatar")) should matchPattern:
          case Some(UnstructuredMovie("Color", "James Cameron", Some(723), Some(178), _, _, "Joel David Moore", _, Some(760505847L), "Action|Adventure|Fantasy|Sci-Fi", "CCH Pounder", _, _, _, _, _, _, _, _, "English", "USA", "PG-13", Some(237000000L), Some(2009), _, 7.9, Some(1.78), Some(33000))) =>

  it should "render a movie as it was read" in:
    matchTry(Table.parseResource[Table[UnstructuredMovie]]("movie_metadata.csv", classOf[UnstructuredMovie])):
      case mt@HeadedTable(_, _) =>
        mt.find(_.movieTitle.startsWith("Avatar")) map (summon[CsvProduct[UnstructuredMovie]].render(_)) shouldBe Some(avatar)

  it should "generate the header of the file it was read from" in:
    // The column names come from the field names, mapped by the same ColumnHelper that parsing uses.
    summon[CsvProduct[UnstructuredMovie]].asInstanceOf[CsvProductGenerator[UnstructuredMovie]].toColumnNames(None, None) shouldBe header
