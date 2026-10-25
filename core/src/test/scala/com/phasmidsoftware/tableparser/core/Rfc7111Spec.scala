package com.phasmidsoftware.tableparser.core

import com.phasmidsoftware.tableparser.core.parse.RawParsers.WithHeaderRow.given
import com.phasmidsoftware.tableparser.core.parse.{CellParser, CellParsers, HeadedCSVTableParser, StandardRowParser, StringParser}
import com.phasmidsoftware.tableparser.core.table._
import java.io.File
import java.nio.file.Files
import java.time.LocalDate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
 * Tests of RFC 7111 (https://www.rfc-editor.org/rfc/rfc7111): fragment identifiers for CSV,
 * using the example table of the RFC (one header line and six rows of data).
 */
class Rfc7111Spec extends AnyFlatSpec with should.Matchers:

  import Rfc7111Spec._

  private val raw: RawTable = Table.parseRaw(example).get

  private def rows(t: RawTable): Seq[String] = t.toSeq.map(_.ws.mkString(","))

  private def fragment(w: String): CsvFragment = CsvFragment.parse(w).get

  behavior of "CsvFragment.parse (RFC 7111, section 3)"

  it should "parse row, column and cell selections" in:
    CsvFragment.parse("row=4") shouldBe Some(RowSelection(Seq(Span(At(4), At(4)))))
    CsvFragment.parse("row=5-*") shouldBe Some(RowSelection(Seq(Span(At(5), Last))))
    CsvFragment.parse("col=1-2") shouldBe Some(ColumnSelection(Seq(Span(At(1), At(2)))))
    CsvFragment.parse("cell=4,1-6,2") shouldBe Some(CellSelection(Seq((Span(At(4), At(6)), Span(At(1), At(2))))))
    CsvFragment.parse("row=3;6") shouldBe Some(RowSelection(Seq(Span(At(3), At(3)), Span(At(6), At(6)))))

  // RFC 7111, section 4.1: a fragment identifier with a syntax error must be ignored.
  it should "reject a fragment identifier with a syntax error" in:
    for w <- Seq("", "row=", "rows=1", "Row=1", "row=1-", "row=a", "row=-1", "row=1;col=2", "cell=4", "cell=4,1-6", "row= 1", "row=1 ") do
      CsvFragment.parse(w) shouldBe None

  behavior of "row selection (RFC 7111, section 2.1)"

  it should "select a row (the header is row 1)" in:
    rows(fragment("row=4").asInstanceOf[RowSelection].select(raw)) shouldBe Seq("2011-01-03,0,Galway")

  it should "select a range of rows, also to the last (*)" in:
    val berkeley = Seq("2011-01-01,6,Berkeley", "2011-01-02,8,Berkeley", "2011-01-03,5,Berkeley")
    rows(fragment("row=5-7").asInstanceOf[RowSelection].select(raw)) shouldBe berkeley
    rows(fragment("row=5-*").asInstanceOf[RowSelection].select(raw)) shouldBe berkeley
    rows(fragment("row=*").asInstanceOf[RowSelection].select(raw)) shouldBe Seq("2011-01-03,5,Berkeley")

  it should "keep the header" in:
    fragment("row=4").asInstanceOf[RowSelection].select(raw).maybeHeader shouldBe raw.maybeHeader

  // RFC 7111, sections 2.4 and 4.2.
  it should "select several rows, ignoring a specification which is inverse or beyond the table" in:
    rows(fragment("row=3;6").asInstanceOf[RowSelection].select(raw)) shouldBe Seq("2011-01-02,-1,Galway", "2011-01-02,8,Berkeley")
    rows(fragment("row=1-2;5-4;13-16").asInstanceOf[RowSelection].select(raw)) shouldBe Seq("2011-01-01,1,Galway")
    rows(fragment("row=6-100").asInstanceOf[RowSelection].select(raw)) shouldBe Seq("2011-01-02,8,Berkeley", "2011-01-03,5,Berkeley")

  it should "select each row once, when specifications overlap" in:
    rows(fragment("row=3-6;4-5").asInstanceOf[RowSelection].select(raw)).size shouldBe 4

  it should "select rows of a table of a case class" in:
    given HeadedCSVTableParser[Reading] = ReadingTableParser
    val readings = Table.parse[Table[Reading]](example).get
    fragment("row=2;7").asInstanceOf[RowSelection].select(readings).toSeq shouldBe
            Seq(Reading(LocalDate.of(2011, 1, 1), 1, "Galway"), Reading(LocalDate.of(2011, 1, 3), 5, "Berkeley"))

  it should "select rows lazily, reading no further than it must (here, from endless input)" in:
    val endless = Iterator.from(1)
    fragment("row=3-4").asInstanceOf[RowSelection].select(endless, 1).toList shouldBe List(2, 3)

  behavior of "column selection (RFC 7111, section 2.2)"

  it should "select a column, with its name" in:
    val t = fragment("col=2").asInstanceOf[ColumnSelection].select(raw)
    t.maybeHeader.map(_.xs) shouldBe Some(Seq("temperature"))
    rows(t) shouldBe Seq("1", "-1", "0", "6", "8", "5")

  it should "select a range of columns, also the last (*)" in:
    fragment("col=1-2").asInstanceOf[ColumnSelection].select(raw).maybeHeader.map(_.xs) shouldBe Some(Seq("date", "temperature"))
    rows(fragment("col=*").asInstanceOf[ColumnSelection].select(raw)).distinct shouldBe Seq("Galway", "Berkeley")

  behavior of "cell selection (RFC 7111, section 2.3)"

  it should "select a cell" in:
    fragment("cell=4,1").asInstanceOf[CellSelection].select(raw).map(rows) shouldBe Seq(Seq("2011-01-03"))

  it should "select a range of cells" in:
    fragment("cell=4,1-6,2").asInstanceOf[CellSelection].select(raw).map(rows) shouldBe Seq(Seq("2011-01-03,0", "2011-01-01,6", "2011-01-02,8"))

  it should "select several ranges of cells, one table for each, leaving out any which selects nothing" in:
    fragment("cell=2,3;9,9;7,1-7,2").asInstanceOf[CellSelection].select(raw).map(rows) shouldBe Seq(Seq("Galway"), Seq("2011-01-03,5"))

  behavior of "Table.parse with a URI"

  it should "parse only the rows selected by the URI's fragment" in:
    val file = File.createTempFile("rfc7111", ".csv")
    file.deleteOnExit()
    Files.writeString(file.toPath, example.mkString("\n"))
    val uri = file.toURI
    Table.parse[RawTable](new java.net.URI(s"$uri#row=5-7")).map(rows) shouldBe scala.util.Success(Seq("2011-01-01,6,Berkeley", "2011-01-02,8,Berkeley", "2011-01-03,5,Berkeley"))
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
