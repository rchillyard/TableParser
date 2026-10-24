package com.phasmidsoftware.tableparser.core.table

import com.phasmidsoftware.tableparser.core.parse.RawParsers.WithHeaderRow.given
import com.phasmidsoftware.tableparser.core.parse.{ParserException, TableParser, TableParserException}
import com.phasmidsoftware.tableparser.core.render.CsvProduct
import java.io.File
import java.nio.file.Files
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Random, Success}

/**
 * Tests of the methods of Table (and its companion) which the other specs don't reach.
 */
class TableMethodsSpec extends AnyFlatSpec with should.Matchers:

  import TableMethodsSpec._

  private val points: Table[Point] = Table(Seq(Point(1, 2), Point(3, 4), Point(5, 6)), Some(Header.create("x", "y")))

  private def tempFile: File =
    val f = File.createTempFile("table", ".csv")
    f.deleteOnExit()
    f

  behavior of "Table methods"

  it should "processRowsMap, filterValid, filterByKey and sample" in:
    points.processRowsMap(_.x).toSeq shouldBe Seq(1, 3, 5)
    given Validity[Point] = (p: Point) => p.x > 1
    points.filterValid.toSeq shouldBe Seq(Point(3, 4), Point(5, 6))
    given HasKey[Point] = (p: Point) => p.x.toString
    points.filterByKey(_ == "3").toSeq shouldBe Seq(Point(3, 4))
    given Random = new Random(0)
    points.sample(1).size should be <= points.size

  // Up to 2.0.2, writeCSVFile never closed the file (nor reported a failure), so what it wrote might not be there.
  it should "writeCSVFile and writeCSVPath, then read back" in:
    for write <- Seq((t: Table[Point], f: File) => t.writeCSVFile(f.toPath), (t: Table[Point], f: File) => t.writeCSVPath(f.toPath)) do
      val file = tempFile
      write(points, file)
      Files.readString(file.toPath) shouldBe "x,y\r\n1,2\r\n3,4\r\n5,6\r\n"

  it should "writeCSVFileRow, then read back" in:
    val file = tempFile
    val raw = Table.parseRaw(Seq("x,y", "1,2", "3,4")).get
    val rows: Table[Row] = raw.map(r => Row(r.ws, r.header, 0))
    Table.writeCSVFileRow(rows, file)
    Files.readString(file.toPath) shouldBe "x,y\r\n1,2\r\n3,4\r\n"

  it should "fail to write a Table of Row which has no header" in:
    val rows: Table[Row] = Table(Seq(Row(Seq("1", "2"), Header.create("x", "y"), 0)), None)
    a[TableException] should be thrownBy Table.toCSVRow(rows)

  behavior of "Table object methods"

  it should "parse raw tables from a path, a resource, and strings" in:
    val file = tempFile
    Files.writeString(file.toPath, "x,y\n1,2\n")
    Table.parsePathRaw(file.toPath, TableParser.includeAll).map(_.toSeq.map(_.ws)) shouldBe Success(Seq(Seq("1", "2")))
    Table.parseRaw(Seq("x,y", "1,2")).map(_.toSeq.map(_.ws)) shouldBe Success(Seq(Seq("1", "2")))
    Table.parseResourceRaw("intPairs.csv", clazz = classOf[TableMethodsSpec]).map(_.size) shouldBe Success(1)

  it should "fail to parseSequence with a parser of Strings" in:
    Table.parseSequence[RawTable](Iterator(Seq("x", "y"), Seq("1", "2"))) should matchPattern { case Failure(_: ParserException) => }

  it should "fail to parse a missing resource" in:
    Table.sourceFromClassResource("no-such-resource.csv", classOf[TableMethodsSpec]) should matchPattern { case Failure(_: TableParserException) => }

  it should "open a file by name" in:
    val file = tempFile
    Files.writeString(file.toPath, "x,y\n1,2\n")
    Table.sourceFromFilename(file.getPath).map(_.getLines().toList) shouldBe Success(List("x,y", "1,2"))

object TableMethodsSpec:
  case class Point(x: Int, y: Int) derives CsvProduct
