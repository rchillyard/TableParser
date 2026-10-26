package com.phasmidsoftware.tableparser.core

import com.phasmidsoftware.tableparser.core.parse.RawParsers.WithHeaderRow.given
import com.phasmidsoftware.tableparser.core.parse.{CellParser, CellParsers, HeadedCSVTableParser, StandardRowParser, StringParser}
import com.phasmidsoftware.tableparser.core.table._
import java.io.File
import java.nio.file.Files
import java.time.LocalDate
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.matchers.should

/**
 * Tests of RFC 7111 (https://www.rfc-editor.org/rfc/rfc7111): fragment identifiers for CSV,
 * using the example table of the RFC (one header line and six rows of data).
 */
class Rfc7111Spec extends AnyFeatureSpec with GivenWhenThen with should.Matchers:

  import Rfc7111Spec._

  private val raw: RawTable = Table.parseRaw(example).get

  private def rows(t: RawTable): Seq[String] = t.toSeq.map(_.ws.mkString(","))

  private def fragment(w: String): CsvFragment = CsvFragment.parse(w).get

  private def selectRows(w: String): Seq[String] =
    Given(s"the RFC's example table, and the fragment identifier #$w")
    When("its rows are selected")
    rows(fragment(w).asInstanceOf[RowSelection].select(raw))

  Feature("Fragment identifiers (RFC 7111, section 3)"):

    Scenario("Row, column and cell selections are parsed"):
      Given("fragment identifiers such as row=4, col=1-2 and cell=4,1-6,2")
      When("they are parsed")
      Then("each is a selection of spans (from the first position wanted, until the first not wanted)")
      // NOTE a Span's until is the first position not wanted: "row=4" is Span(At(4), At(5)).
      CsvFragment.parse("row=4") shouldBe Some(RowSelection(Seq(Span(At(4), At(5)))))
      CsvFragment.parse("row=5-*") shouldBe Some(RowSelection(Seq(Span(At(5), Last))))
      CsvFragment.parse("row=*") shouldBe Some(RowSelection(Seq(Span(Last, Last))))
      CsvFragment.parse("col=1-2") shouldBe Some(ColumnSelection(Seq(Span(At(1), At(3)))))
      CsvFragment.parse("cell=4,1-6,2") shouldBe Some(CellSelection(Seq((Span(At(4), At(7)), Span(At(1), At(3))))))
      CsvFragment.parse("row=3;6") shouldBe Some(RowSelection(Seq(Span(At(3), At(4)), Span(At(6), At(7)))))
      And("a number too large for an Int means the last")
      CsvFragment.parse("row=1-99999999999") shouldBe Some(RowSelection(Seq(Span(At(1), Last))))

    // RFC 7111, section 4.1: a fragment identifier with a syntax error must be ignored.
    Scenario("A fragment identifier with a syntax error is ignored (section 4.1)"):
      val bad = Seq("", "row=", "rows=1", "Row=1", "row=1-", "row=a", "row=-1", "row=1;col=2", "cell=4", "cell=4,1-6", "row= 1", "row=1 ")
      Given(s"the fragment identifiers ${bad.map(w => s"\"$w\"").mkString(", ")}")
      When("they are parsed")
      Then("none is a selection")
      for w <- bad do CsvFragment.parse(w) shouldBe None

  Feature("Spans"):

    Scenario("A Span contains from up to, but not including, until"):
      Given("the Span from 5 until 8")
      Then("it contains 5, 6 and 7, but not 4 or 8")
      Span(At(5), At(8)).contains(4, None) shouldBe false
      (5 to 7) forall (Span(At(5), At(8)).contains(_, None)) shouldBe true
      Span(At(5), At(8)).contains(8, None) shouldBe false
      And("the Span from 5 until 5 is empty")
      Span(At(5), At(5)).contains(5, None) shouldBe false
      And("the Span from 5 until the last contains 1000")
      Span(At(5), Last).contains(1000, None) shouldBe true
      And("the Span of the last contains only the last")
      Span(Last, Last).contains(7, Some(7)) shouldBe true
      Span(Last, Last).contains(6, Some(7)) shouldBe false
      And("the RFC's inclusive range 5-7 is the Span from 5 until 8")
      Span.inclusive(At(5), At(7)) shouldBe Span(At(5), At(8))

  Feature("Selecting rows (RFC 7111, section 2.1)"):

    Scenario("A row is selected (the header is row 1)"):
      val selected = selectRows("row=4")
      Then("the row is the third row of data")
      selected shouldBe Seq("2011-01-03,0,Galway")

    Scenario("A range of rows is selected, also to the last (*)"):
      val berkeley = Seq("2011-01-01,6,Berkeley", "2011-01-02,8,Berkeley", "2011-01-03,5,Berkeley")
      selectRows("row=5-7") shouldBe berkeley
      Then("the rows are those of Berkeley")
      selectRows("row=5-*") shouldBe berkeley
      Then("the rows are those of Berkeley")
      selectRows("row=*") shouldBe Seq("2011-01-03,5,Berkeley")
      Then("the row is the last")

    Scenario("The header is kept"):
      Given("the RFC's example table, and the fragment identifier #row=4")
      When("its rows are selected")
      Then("the selection has the same header")
      fragment("row=4").asInstanceOf[RowSelection].select(raw).maybeHeader shouldBe raw.maybeHeader

    // RFC 7111, sections 2.4 and 4.2.
    Scenario("Several rows are selected, ignoring a specification which is inverse or beyond the table (sections 2.4 and 4.2)"):
      selectRows("row=3;6") shouldBe Seq("2011-01-02,-1,Galway", "2011-01-02,8,Berkeley")
      Then("both rows are selected")
      selectRows("row=1-2;5-4;13-16") shouldBe Seq("2011-01-01,1,Galway")
      Then("only the first row of data is selected (5-4 is inverse, and 13-16 is beyond the table)")
      selectRows("row=6-100") shouldBe Seq("2011-01-02,8,Berkeley", "2011-01-03,5,Berkeley")
      Then("the range is cut short at the end of the table")

    Scenario("Each row is selected once, when specifications overlap"):
      val selected = selectRows("row=3-6;4-5")
      Then("four rows are selected")
      selected.size shouldBe 4

    Scenario("Rows are selected from a table of a case class"):
      Given("the RFC's example table, parsed as Readings, and the fragment identifier #row=2;7")
      given HeadedCSVTableParser[Reading] = ReadingTableParser
      val readings = Table.parse[Table[Reading]](example).get
      When("its rows are selected")
      Then("the first and last Readings are selected")
      fragment("row=2;7").asInstanceOf[RowSelection].select(readings).toSeq shouldBe
              Seq(Reading(LocalDate.of(2011, 1, 1), 1, "Galway"), Reading(LocalDate.of(2011, 1, 3), 5, "Berkeley"))

    Scenario("Rows are selected lazily, reading no further than necessary"):
      Given("endless input (1, 2, 3, ...), after one header line, and the fragment identifier #row=3-4")
      val endless = Iterator.from(1)
      When("its rows are selected")
      Then("the selection is 2 and 3, and it ends")
      fragment("row=3-4").asInstanceOf[RowSelection].select(endless, 1).toList shouldBe List(2, 3)

  Feature("Selecting columns (RFC 7111, section 2.2)"):

    Scenario("A column is selected, with its name"):
      Given("the RFC's example table, and the fragment identifier #col=2")
      When("its columns are selected")
      val t = fragment("col=2").asInstanceOf[ColumnSelection].select(raw)
      Then("the column is temperature")
      t.maybeHeader.map(_.xs) shouldBe Some(Seq("temperature"))
      rows(t) shouldBe Seq("1", "-1", "0", "6", "8", "5")

    Scenario("A range of columns is selected, also the last (*)"):
      Given("the RFC's example table, and the fragment identifier #col=1-2")
      When("its columns are selected")
      Then("the columns are date and temperature")
      fragment("col=1-2").asInstanceOf[ColumnSelection].select(raw).maybeHeader.map(_.xs) shouldBe Some(Seq("date", "temperature"))
      And("#col=* selects the column of places")
      rows(fragment("col=*").asInstanceOf[ColumnSelection].select(raw)).distinct shouldBe Seq("Galway", "Berkeley")

  Feature("Selecting cells (RFC 7111, section 2.3)"):

    Scenario("A cell is selected"):
      Given("the RFC's example table, and the fragment identifier #cell=4,1")
      When("its cells are selected")
      Then("the cell is the date of the third row of data")
      fragment("cell=4,1").asInstanceOf[CellSelection].select(raw).map(rows) shouldBe Seq(Seq("2011-01-03"))

    Scenario("A range of cells is selected"):
      Given("the RFC's example table, and the fragment identifier #cell=4,1-6,2")
      When("its cells are selected")
      Then("the cells are the dates and temperatures of rows 4 to 6")
      fragment("cell=4,1-6,2").asInstanceOf[CellSelection].select(raw).map(rows) shouldBe Seq(Seq("2011-01-03,0", "2011-01-01,6", "2011-01-02,8"))

    Scenario("Several ranges of cells are selected, one table for each, leaving out any which selects nothing"):
      Given("the RFC's example table, and the fragment identifier #cell=2,3;9,9;7,1-7,2")
      When("its cells are selected")
      Then("there are two tables (9,9 is beyond the table)")
      fragment("cell=2,3;9,9;7,1-7,2").asInstanceOf[CellSelection].select(raw).map(rows) shouldBe Seq(Seq("Galway"), Seq("2011-01-03,5"))

  Feature("Parsing a URI with a fragment identifier"):

    Scenario("Only the rows selected by the URI's fragment are parsed"):
      Given("a file which holds the RFC's example table")
      val file = File.createTempFile("rfc7111", ".csv")
      file.deleteOnExit()
      Files.writeString(file.toPath, example.mkString("\n"))
      val uri = file.toURI
      When("it is parsed with the fragment identifier #row=5-7")
      Then("the rows are those of Berkeley")
      Table.parse[RawTable](new java.net.URI(s"$uri#row=5-7")).map(rows) shouldBe scala.util.Success(Seq("2011-01-01,6,Berkeley", "2011-01-02,8,Berkeley", "2011-01-03,5,Berkeley"))
      And("with a fragment identifier which is not valid, or which doesn't select rows, or with none, all six rows are parsed")
      // Up to 2.0.3, a URI with a fragment could not be parsed at all.
      Table.parse[RawTable](new java.net.URI(s"$uri#row=not-valid")).map(_.size) shouldBe scala.util.Success(6)
      Table.parse[RawTable](new java.net.URI(s"$uri#col=2")).map(_.size) shouldBe scala.util.Success(6)
      Table.parse[RawTable](uri).map(_.size) shouldBe scala.util.Success(6)

object Rfc7111Spec:
  // The example table of RFC 7111, section 2.
  val example: Seq[String] = Seq(
    "date,temperature,place"
    , "2011-01-01,1,Galway"
    , "2011-01-02,-1,Galway"
    , "2011-01-03,0,Galway"
    , "2011-01-01,6,Berkeley"
    , "2011-01-02,8,Berkeley"
    , "2011-01-03,5,Berkeley"
  )

  case class Reading(date: LocalDate, temperature: Int, place: String)

  object Reading extends CellParsers:
    given CellParser[Reading] = CellParser.derived[Reading]

    given StringParser[Reading] = StandardRowParser.create[Reading]

  object ReadingTableParser extends HeadedCSVTableParser[Reading]
