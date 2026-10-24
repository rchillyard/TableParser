package com.phasmidsoftware.tableparser.core

import com.phasmidsoftware.tableparser.core.parse._
import com.phasmidsoftware.tableparser.core.render.{CsvProduct, CsvRenderer, CsvRenderers}
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.util.EvaluateTry.matchTry
import org.scalatest.flatspec.AnyFlatSpec
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
 * Each test below is one rule of section 2 of the RFC, for reading (with the default RowConfig)
 * or for writing (with the default CsvAttributes).
 */
class Rfc4180Spec extends AnyFlatSpec with Matchers:

  // Reads CSV text (as a file would be read) as a table of raw rows, and gives the fields of each row.
  private def read(csv: String, parser: RawTableParser = RawTableParser()): Try[Seq[Seq[String]]] =
    implicit val p: RawTableParser = parser
    for (t <- Table.parse[RawTable](Try(Source.fromString(csv)))) yield t.toSeq.map(_.ws)

  behavior of "reading (RFC 4180, section 2)"

  it should "read records separated by CRLF, or by LF (rule 1)" in:
    read("a,b\r\nx,y\r\nz,w\r\n") shouldBe Success(Seq(Seq("x", "y"), Seq("z", "w")))
    read("a,b\nx,y\nz,w\n") shouldBe Success(Seq(Seq("x", "y"), Seq("z", "w")))

  it should "read a last record which has no line break (rule 2)" in:
    read("a,b\r\nx,y") shouldBe Success(Seq(Seq("x", "y")))

  it should "take the column names from the header line (rule 3)" in:
    implicit val p: RawTableParser = RawTableParser()
    matchTry(Table.parse[RawTable](Try(Source.fromString("name,\"rating, 2026\"\r\nRobin,1500\r\n")))):
      case t@HeadedTable(_, h) =>
        h.xs shouldBe Seq("name", "rating, 2026")
        t.head.ws shouldBe Seq("Robin", "1500")

  it should "keep spaces, which are part of a field (rule 4)" in:
    read("a,b,c\r\nx, y , z\r\n") shouldBe Success(Seq(Seq("x", " y ", " z")))

  it should "read fields which are quoted, or not (rule 5)" in:
    read("a,b,c\r\n\"aaa\",\"bbb\",ccc\r\n") shouldBe Success(Seq(Seq("aaa", "bbb", "ccc")))

  it should "read quoted fields which contain commas or line breaks (rule 6)" in:
    read("a,b,c\r\n\"aaa\",\"b,bb\",\"c\r\ncc\"\r\nzzz,yyy,xxx\r\n") shouldBe Success(Seq(Seq("aaa", "b,bb", "c\ncc"), Seq("zzz", "yyy", "xxx")))

  it should "read quoted fields which contain quotes, which are doubled (rule 7)" in:
    read("a,b,c\r\n\"aaa\",\"b\"\"bb\",\"ccc\"\r\n") shouldBe Success(Seq(Seq("aaa", "b\"bb", "ccc")))

  it should "ignore spaces next to commas with RowConfig.trimmingRowConfig (not RFC 4180, but as 1.x did)" in:
    val trimming =
      implicit val rowConfig: RowConfig = RowConfig.trimmingRowConfig
      implicit val rawRowParser: CellParser[RawRow] = StdCellParsers.rawRowCellParser
      RawTableParser().setRowParser(StandardRowParser.create[RawRow])
    read("a, b\r\nx, y\r\n", trimming) shouldBe Success(Seq(Seq("x", "y")))

  behavior of "writing (RFC 4180, section 2)"

  private val awkward = Seq(
      Note(1, "plain")
    , Note(2, "a, comma")
    , Note(3, "a \"quote\"")
    , Note(4, "a line\r\nbreak")
    , Note(5, " spaces ")
    , Note(6, "")
  )

  it should "write each field so that it can be read back as it is, quoting it if necessary (rules 4 to 7)" in:
    val renderer = summon[CsvRenderer[Note]]
    awkward map (renderer.render(_)) shouldBe Seq("1,plain", "2,\"a, comma\"", "3,\"a \"\"quote\"\"\"", "4,\"a line\r\nbreak\"", "5,\" spaces \"", "6,")

  it should "write records separated by CRLF, with a header line (rules 1 and 3)" in:
    Table(awkward.take(2), Some(Header.create("id", "text"))).toCSV shouldBe Success("id,text\r\n1,plain\r\n2,\"a, comma\"\r\n")

  it should "write records separated by LF, if the CsvAttributes say so" in:
    implicit val lf: CsvAttributes = CsvAttributes(",", "\"", "\n")
    implicit val renderer: CsvRenderer[Note] = CsvRenderer.derived[Note]
    implicit val generator: render.CsvGenerator[Note] = render.CsvGenerator.derived[Note]
    Table(awkward.take(1), Some(Header.create("id", "text"))).toCSV shouldBe Success("id,text\n1,plain\n")

  it should "quote a column name which needs it" in:
    CsvRenderers.quoteIfNecessary("rating, 2026") shouldBe "\"rating, 2026\""
    CsvRenderers.quoteIfNecessary("rating") shouldBe "rating"

  behavior of "round trip"

  it should "read back exactly what it writes" in:
    val result = for {
      csv <- Table(awkward, Some(Header.create("id", "text"))).toCSV
      again <- Table.parse[Table[Note]](Try(Source.fromString(csv)))
    } yield again.toSeq
    // NOTE a line break inside a field is read back as LF (Source.getLines splits CRLF and LF alike).
    result shouldBe Success(awkward.map(n => n.copy(text = n.text.replace("\r\n", "\n"))))
