package com.phasmidsoftware.tableparser.cats.table

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.tableparser.core.parse.RawParsers.WithHeaderRow.given
import com.phasmidsoftware.tableparser.core.parse.TableParser
import com.phasmidsoftware.tableparser.core.render.CsvProduct
import com.phasmidsoftware.tableparser.core.table._
import java.io.{ByteArrayInputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.io.Source

/**
 * Tests of each method of IOTable, mostly with a small file of points (points.csv: x,y then 1,2 and 3,4).
 */
class IOTableSpec extends AnyFlatSpec with should.Matchers {

  import IOTableSpec._

  private def rows(io: IO[RawTable]): Seq[Seq[String]] = io.unsafeRunSync().toSeq.map(_.ws)

  private val expected = Seq(Seq("1", "2"), Seq("3", "4"))

  private lazy val file: File = {
    val f = File.createTempFile("points", ".csv")
    f.deleteOnExit()
    Files.writeString(f.toPath, csv)
    f
  }

  behavior of "IOTable.parse"

  it should "parse an Iterable of String" in {
    rows(IOTable.parse[RawTable](csv.split("\n").toSeq)) shouldBe expected
  }

  it should "parse an Iterator of String" in {
    rows(IOTable.parse[RawTable](csv.split("\n").iterator, 1)) shouldBe expected
  }

  it should "parse a Source" in {
    rows(IOTable.parseSource[RawTable](Source.fromString(csv))) shouldBe expected
    rows(IOTable.parse[RawTable](IO(Source.fromString(csv)))) shouldBe expected
  }

  it should "parse a URI" in {
    rows(IOTable.parse[RawTable](file.toURI)) shouldBe expected
    rows(IOTable.parse[RawTable](file.toURI, "UTF-8")) shouldBe expected
  }

  it should "parse an InputStream (using the given encoding)" in {
    rows(IOTable.parseInputStream[RawTable](new ByteArrayInputStream(csv.getBytes(StandardCharsets.UTF_8)))) shouldBe expected
    val utf16 = new ByteArrayInputStream("x,y\nZoë,2\n".getBytes(StandardCharsets.UTF_16))
    rows(IOTable.parseInputStream[RawTable](IO(utf16), "UTF-16")) shouldBe Seq(Seq("Zoë", "2"))
  }

  it should "parse a File" in {
    rows(IOTable.parseFile[RawTable](file)) shouldBe expected
    rows(IOTable.parseFile[RawTable](file, "UTF-8")) shouldBe expected
    rows(IOTable.parseFile[RawTable](file.getPath)) shouldBe expected
    rows(IOTable.parseFile[RawTable](file.getPath, "UTF-8")) shouldBe expected
  }

  it should "parse a resource" in {
    rows(IOTable.parseResource[RawTable]("points.csv", classOf[IOTableSpec])) shouldBe expected
    val url = classOf[IOTableSpec].getResource("points.csv")
    rows(IOTable.parseResource[RawTable](url)) shouldBe expected
    rows(IOTable.parseResource[RawTable](url, "UTF-8")) shouldBe expected
  }

  it should "fail for a missing resource" in {
    an[Exception] should be thrownBy IOTable.parseResource[RawTable]("nonexistent.csv", classOf[IOTableSpec]).unsafeRunSync()
  }

  behavior of "IOTable raw parsing"

  it should "parseRaw" in {
    rows(IOTable.parseRaw(csv.split("\n").toSeq)) shouldBe expected
  }

  it should "parseRaw with a predicate and a fixed header" in {
    val table = IOTable.parseRaw(Seq("1,2", "3,4"), _.map(_.ws.head == "3").getOrElse(false), Some(Header.create("x", "y"))).unsafeRunSync()
    table.toSeq.map(_.ws) shouldBe Seq(Seq("3", "4"))
    table.maybeHeader shouldBe Some(Header.create("x", "y"))
  }

  it should "parseFileRaw" in {
    rows(IOTable.parseFileRaw(file, TableParser.includeAll)) shouldBe expected
    rows(IOTable.parseFileRaw(file.getPath, TableParser.includeAll)) shouldBe expected
  }

  it should "parseResourceRaw" in {
    rows(IOTable.parseResourceRaw("points.csv", clazz = classOf[IOTableSpec])) shouldBe expected
  }

  behavior of "IOTable output"

  it should "make a Table with or without a header" in {
    IOTable(Seq(1, 2), Some(Header.create("n"))) should matchPattern { case HeadedTable(_, _) => }
    IOTable(Seq(1, 2), None) should matchPattern { case UnheadedTable(_) => }
  }

  it should "render a Table as CSV" in {
    val table = Table(Seq(Point(1, 2), Point(3, 4)), Some(Header.create("x", "y")))
    IOTable.toCSV(table).unsafeRunSync() shouldBe "x,y\r\n1,2\r\n3,4\r\n"
  }

  it should "render a Table of Row as CSV" in {
    IOTable.toCSVRow(pointRows).unsafeRunSync() shouldBe "x,y\r\n1,2\r\n3,4\r\n"
  }

  it should "write a Table of Row to a file, and read it back" in {
    val out = File.createTempFile("points-out", ".csv")
    out.deleteOnExit()
    IOTable.writeCSVFileRow(pointRows, out).unsafeRunSync()
    rows(IOTable.parseFile[RawTable](out)) shouldBe expected
  }

  it should "fail (in IO) to write a Table of Row which has no header" in {
    val table: Table[Row] = IOTable(Seq(Row(Seq("1", "2"), Header.create("x", "y"), 0)), None)
    IOTable.toCSVRow(table).attempt.unsafeRunSync() should matchPattern { case Left(_: TableException) => }
    IOTable.writeCSVFileRow(table, file).attempt.unsafeRunSync() should matchPattern { case Left(_: TableException) => }
  }
}

object IOTableSpec {
  val csv: String = "x,y\n1,2\n3,4\n"

  case class Point(x: Int, y: Int) derives CsvProduct

  private val hdr = Header.create("x", "y")

  val pointRows: Table[Row] = Table(Seq(Row(Seq("1", "2"), hdr, 1), Row(Seq("3", "4"), hdr, 2)), Some(hdr))
}
