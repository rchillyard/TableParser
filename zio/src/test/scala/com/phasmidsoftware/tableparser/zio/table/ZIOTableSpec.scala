package com.phasmidsoftware.tableparser.zio.table

import com.phasmidsoftware.tableparser.core.parse.RawParsers.WithHeaderRow.given
import com.phasmidsoftware.tableparser.core.parse.TableParser
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.zio.util.EvaluateZIO
import java.io.{ByteArrayInputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.io.Source
import zio.{Task, ZIO}

/**
 * Tests of each method of ZIOTable (as for IOTable in the cats module), mostly with a small file of points (points.csv: x,y then 1,2 and 3,4).
 */
class ZIOTableSpec extends AnyFlatSpec with should.Matchers {

  import ZIOTableSpec._

  private def rows(task: Task[RawTable]): Seq[Seq[String]] = run(task).toSeq.map(_.ws)

  private def run[X](task: Task[X]): X = EvaluateZIO.run(task).fold(e => throw e, identity)

  private val expected = Seq(Seq("1", "2"), Seq("3", "4"))

  private lazy val file: File = {
    val f = File.createTempFile("points", ".csv")
    f.deleteOnExit()
    Files.writeString(f.toPath, csv)
    f
  }

  behavior of "ZIOTable.parse"

  it should "parse an Iterable of String" in {
    rows(ZIOTable.parse[RawTable](csv.split("\n").toSeq)) shouldBe expected
  }

  it should "parse an Iterator of String" in {
    rows(ZIOTable.parse[RawTable](csv.split("\n").iterator, 1)) shouldBe expected
  }

  it should "parse a Source" in {
    rows(ZIOTable.parseSource[RawTable](Source.fromString(csv))) shouldBe expected
    rows(ZIOTable.parse[RawTable](ZIO.succeed(Source.fromString(csv)))) shouldBe expected
  }

  it should "parse a URI" in {
    rows(ZIOTable.parse[RawTable](file.toURI)) shouldBe expected
    rows(ZIOTable.parse[RawTable](file.toURI, "UTF-8")) shouldBe expected
  }

  it should "parse an InputStream (using the given encoding)" in {
    rows(ZIOTable.parseInputStream[RawTable](new ByteArrayInputStream(csv.getBytes(StandardCharsets.UTF_8)))) shouldBe expected
    val utf16 = new ByteArrayInputStream("x,y\nZoë,2\n".getBytes(StandardCharsets.UTF_16))
    rows(ZIOTable.parseInputStream[RawTable](ZIO.succeed(utf16), "UTF-16")) shouldBe Seq(Seq("Zoë", "2"))
  }

  it should "parse a File" in {
    rows(ZIOTable.parseFile[RawTable](file)) shouldBe expected
    rows(ZIOTable.parseFile[RawTable](file, "UTF-8")) shouldBe expected
    rows(ZIOTable.parseFile[RawTable](file.getPath)) shouldBe expected
    rows(ZIOTable.parseFile[RawTable](file.getPath, "UTF-8")) shouldBe expected
  }

  it should "parse a resource" in {
    rows(ZIOTable.parseResource[RawTable]("points.csv", classOf[ZIOTableSpec])) shouldBe expected
    val url = classOf[ZIOTableSpec].getResource("points.csv")
    rows(ZIOTable.parseResource[RawTable](url)) shouldBe expected
    rows(ZIOTable.parseResource[RawTable](url, "UTF-8")) shouldBe expected
  }

  it should "fail for a missing resource" in {
    an[Exception] should be thrownBy run(ZIOTable.parseResource[RawTable]("nonexistent.csv", classOf[ZIOTableSpec]))
  }

  behavior of "ZIOTable raw parsing"

  it should "parseRaw" in {
    rows(ZIOTable.parseRaw(csv.split("\n").toSeq)) shouldBe expected
  }

  it should "parseRaw with a predicate and a fixed header" in {
    val table = run(ZIOTable.parseRaw(Seq("1,2", "3,4"), _.map(_.ws.head == "3").getOrElse(false), Some(Header.create("x", "y"))))
    table.toSeq.map(_.ws) shouldBe Seq(Seq("3", "4"))
    table.maybeHeader shouldBe Some(Header.create("x", "y"))
  }

  it should "parseFileRaw" in {
    rows(ZIOTable.parseFileRaw(file, TableParser.includeAll)) shouldBe expected
    rows(ZIOTable.parseFileRaw(file.getPath, TableParser.includeAll)) shouldBe expected
  }

  it should "parseResourceRaw" in {
    rows(ZIOTable.parseResourceRaw("points.csv", clazz = classOf[ZIOTableSpec])) shouldBe expected
  }

  behavior of "ZIOTable output"

  it should "make a Table with or without a header" in {
    ZIOTable(Seq(1, 2), Some(Header.create("n"))) should matchPattern { case HeadedTable(_, _) => }
    ZIOTable(Seq(1, 2), None) should matchPattern { case UnheadedTable(_) => }
  }

  it should "render a Table of Row as CSV" in {
    run(ZIOTable.toCSVRow(pointRows)) shouldBe "x,y\r\n1,2\r\n3,4\r\n"
  }

  it should "fail (in the Task) to render a Table of Row which has no header" in {
    val table: Table[Row] = ZIOTable(Seq(Row(Seq("1", "2"), Header.create("x", "y"), 0)), None)
    EvaluateZIO.run(ZIOTable.toCSVRow(table)) should matchPattern { case Left(_: TableException) => }
  }
}

object ZIOTableSpec {
  val csv: String = "x,y\n1,2\n3,4\n"

  private val hdr = Header.create("x", "y")

  val pointRows: Table[Row] = Table(Seq(Row(Seq("1", "2"), hdr, 1), Row(Seq("3", "4"), hdr, 2)), Some(hdr))
}
