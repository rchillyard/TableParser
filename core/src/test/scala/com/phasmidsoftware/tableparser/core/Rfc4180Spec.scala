package com.phasmidsoftware.tableparser.core

import com.phasmidsoftware.tableparser.core.parse._
import com.phasmidsoftware.tableparser.core.render.{CsvGenerator, CsvProduct, CsvRenderer, CsvRenderers}
import com.phasmidsoftware.tableparser.core.table._
import org.scalatest.GivenWhenThen
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source
import scala.util.{Success, Try}

// A record for the round trip: its fields can hold anything that RFC 4180 allows in a field.
case class Note(id: Int, text: String) derives CsvProduct

object Note:
  implicit val parser: CellParser[Note] = CellParser.derived[Note]
  implicit val stringParser: StringParser[Note] = StandardRowParser.create[Note]

  implicit object NoteTableParser extends HeadedCSVTableParser[Note]

/**
 * TableParser reads and writes CSV as defined by RFC 4180 (https://www.rfc-editor.org/rfc/rfc4180).
 * Each scenario below is one rule of section 2 of the RFC, for reading (with the default RowConfig)
 * or for writing (with the default CsvAttributes).
 */
class Rfc4180Spec extends AnyFeatureSpec with GivenWhenThen with Matchers:

  import Rfc4180Spec._

  // Reads CSV text (as a file would be read) as a table of raw rows, and gives the fields of each row.
  private def read(csv: String, parser: RawTableParser = RawTableParser()): Try[Seq[Seq[String]]] =
    implicit val p: RawTableParser = parser
    for (t <- Table.parse[RawTable](Try(Source.fromString(csv)))) yield t.toSeq.map(_.ws)

  Feature("Reading CSV (RFC 4180, section 2)"):

    Scenario("Rule 1: records are separated by CRLF (or by LF)"):
      Given(s"the CSV ${show("a,b\r\nx,y\r\nz,w\r\n")}")
      When("it is read")
      Then("there are two records")
      read("a,b\r\nx,y\r\nz,w\r\n") shouldBe Success(Seq(Seq("x", "y"), Seq("z", "w")))
      And(s"the same is true of ${show("a,b\nx,y\nz,w\n")}")
      read("a,b\nx,y\nz,w\n") shouldBe Success(Seq(Seq("x", "y"), Seq("z", "w")))

    Scenario("Rule 2: the last record may have no line break"):
      Given(s"the CSV ${show("a,b\r\nx,y")}")
      When("it is read")
      Then("the last record is x,y")
      read("a,b\r\nx,y") shouldBe Success(Seq(Seq("x", "y")))

    Scenario("Rule 3: the header line names the columns"):
      Given(s"the CSV ${show("name,\"rating, 2026\"\r\nRobin,1500\r\n")}")
      When("it is read")
      implicit val p: RawTableParser = RawTableParser()
      val table = Table.parse[RawTable](Try(Source.fromString("name,\"rating, 2026\"\r\nRobin,1500\r\n")))
      Then("the columns are named name and rating, 2026")
      table.map(_.maybeColumnNames) shouldBe Success(Some(Seq("name", "rating, 2026")))
      And("the record is Robin,1500")
      table.map(_.head.ws) shouldBe Success(Seq("Robin", "1500"))

    Scenario("Rule 4: spaces are part of a field"):
      Given(s"the CSV ${show("a,b,c\r\nx, y , z\r\n")}")
      When("it is read")
      Then("the spaces are kept")
      read("a,b,c\r\nx, y , z\r\n") shouldBe Success(Seq(Seq("x", " y ", " z")))

    Scenario("Rule 5: a field may be quoted, or not"):
      Given(s"the CSV ${show("a,b,c\r\n\"aaa\",\"bbb\",ccc\r\n")}")
      When("it is read")
      Then("the quotes are not part of the fields")
      read("a,b,c\r\n\"aaa\",\"bbb\",ccc\r\n") shouldBe Success(Seq(Seq("aaa", "bbb", "ccc")))

    Scenario("Rule 6: a quoted field may contain commas and line breaks"):
      Given(s"the CSV ${show("a,b,c\r\n\"aaa\",\"b,bb\",\"c\r\ncc\"\r\nzzz,yyy,xxx\r\n")}")
      When("it is read")
      Then("the comma and the line break are part of their fields (the line break is read as LF)")
      read("a,b,c\r\n\"aaa\",\"b,bb\",\"c\r\ncc\"\r\nzzz,yyy,xxx\r\n") shouldBe Success(Seq(Seq("aaa", "b,bb", "c\ncc"), Seq("zzz", "yyy", "xxx")))

    Scenario("Rule 7: a quote inside a quoted field is doubled"):
      Given(s"the CSV ${show("a,b,c\r\n\"aaa\",\"b\"\"bb\",\"ccc\"\r\n")}")
      When("it is read")
      Then("the doubled quote is read as one quote")
      read("a,b,c\r\n\"aaa\",\"b\"\"bb\",\"ccc\"\r\n") shouldBe Success(Seq(Seq("aaa", "b\"bb", "ccc")))

    Scenario("Not RFC 4180, but as 1.x did: spaces next to commas are ignored, with RowConfig.trimmingRowConfig"):
      Given(s"the CSV ${show("a, b\r\nx, y\r\n")}")
      And("a parser whose RowConfig is RowConfig.trimmingRowConfig")
      val trimming =
        implicit val rowConfig: RowConfig = RowConfig.trimmingRowConfig
        implicit val rawRowParser: CellParser[RawRow] = StdCellParsers.rawRowCellParser
        RawTableParser().setRowParser(StandardRowParser.create[RawRow])
      When("it is read")
      Then("the spaces are not part of the fields")
      read("a, b\r\nx, y\r\n", trimming) shouldBe Success(Seq(Seq("x", "y")))

  Feature("Writing CSV (RFC 4180, section 2)"):

    Scenario("Rules 4 to 7: each field is written so that it can be read back as it is, quoted only if necessary"):
      Given("notes whose texts contain a comma, a quote, a line break, spaces, or nothing at all")
      val renderer = summon[CsvRenderer[Note]]
      When("each is written")
      Then("a field is quoted when it contains a comma, a quote or a line break, or starts or ends with a space")
      And("a quote inside it is doubled")
      awkward map (renderer.render(_)) shouldBe Seq("1,plain", "2,\"a, comma\"", "3,\"a \"\"quote\"\"\"", "4,\"a line\r\nbreak\"", "5,\" spaces \"", "6,")

    Scenario("Rules 1 and 3: records are separated by CRLF, after a header line"):
      Given("a table of two notes, with a header")
      When("it is written")
      Then("the header comes first, and each line ends with CRLF")
      Table(awkward.take(2), Some(Header.create("id", "text"))).toCSV shouldBe Success("id,text\r\n1,plain\r\n2,\"a, comma\"\r\n")

    Scenario("Records may be separated by LF instead, if the CsvAttributes say so"):
      Given("CsvAttributes whose newline is LF")
      implicit val lf: CsvAttributes = CsvAttributes(",", "\"", "\n")
      implicit val renderer: CsvRenderer[Note] = CsvRenderer.derived[Note]
      implicit val generator: CsvGenerator[Note] = CsvGenerator.derived[Note]
      When("a table is written")
      Then("each line ends with LF")
      Table(awkward.take(1), Some(Header.create("id", "text"))).toCSV shouldBe Success("id,text\n1,plain\n")

    Scenario("A column name is quoted when it needs to be"):
      Given("the column names \"rating, 2026\" and \"rating\"")
      When("they are written")
      Then("only the first is quoted")
      CsvRenderers.quoteIfNecessary("rating, 2026") shouldBe "\"rating, 2026\""
      CsvRenderers.quoteIfNecessary("rating") shouldBe "rating"

  Feature("Round trip"):

    Scenario("A table that is written is read back exactly"):
      Given("a table of notes whose texts contain everything that RFC 4180 allows in a field")
      When("it is written, and read again")
      val result = for
        csv <- Table(awkward, Some(Header.create("id", "text"))).toCSV
        again <- Table.parse[Table[Note]](Try(Source.fromString(csv)))
      yield again.toSeq
      Then("the notes are the same (except that a line break inside a field is read as LF)")
      // NOTE a line break inside a field is read back as LF (Source.getLines splits CRLF and LF alike).
      result shouldBe Success(awkward.map(n => n.copy(text = n.text.replace("\r\n", "\n"))))

object Rfc4180Spec:
  val awkward: Seq[Note] = Seq(
      Note(1, "plain")
    , Note(2, "a, comma")
    , Note(3, "a \"quote\"")
    , Note(4, "a line\r\nbreak")
    , Note(5, " spaces ")
    , Note(6, "")
  )

  // Shows CSV text in the report, with its line breaks made visible.
  def show(csv: String): String = csv.replace("\r\n", "<CRLF>").replace("\n", "<LF>")
