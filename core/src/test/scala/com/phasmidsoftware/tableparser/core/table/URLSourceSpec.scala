package com.phasmidsoftware.tableparser.core.table

import com.phasmidsoftware.tableparser.core.parse.{CellParser, CellParsers, HeadedCSVTableParser, StandardRowParser, StringParser}
import com.phasmidsoftware.tableparser.core.util.LocalSources
import java.io.File
import java.net.URI
import java.nio.file.Files
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.Success

/**
 * Tests of reading tables from URLs which aren't files (#128): over HTTP, and from a jar.
 * Up to 2.5.0, these methods only accepted file: URIs.
 */
class URLSourceSpec extends AnyFlatSpec with should.Matchers:

  import URLSourceSpec._

  behavior of "Table.parse(uri) and parseResource(url)"

  it should "read a table over HTTP" in:
    LocalSources.withHttpServer(Map("/points.csv" -> csv)): base =>
      val uri = base.resolve("/points.csv")
      Table.parse[Table[Point]](uri).map(_.toSeq) shouldBe Success(points)
      Table.parseResource[Table[Point]](uri.toURL).map(_.toSeq) shouldBe Success(points)

  it should "apply an RFC 7111 fragment to a table read over HTTP" in:
    LocalSources.withHttpServer(Map("/points.csv" -> csv)): base =>
      Table.parse[Table[Point]](URI.create(s"$base/points.csv#row=3-4")).map(_.toSeq) shouldBe Success(points.slice(1, 3))

  it should "read a table from a jar" in:
    Table.parse[Table[Point]](LocalSources.jarURI("points.csv", csv)).map(_.toSeq) shouldBe Success(points)

  it should "still read a file" in:
    Table.parse[Table[Point]](file().toURI).map(_.toSeq) shouldBe Success(points)

  it should "fail for a URL which can't be read" in:
    LocalSources.withHttpServer(Map("/points.csv" -> csv)): base =>
      Table.parse[Table[Point]](base.resolve("/nowhere.csv")).isFailure shouldBe true

  behavior of "Table.rowsURI"

  it should "read the rows of a table over HTTP, applying an RFC 7111 fragment" in:
    LocalSources.withHttpServer(Map("/points.csv" -> csv)): base =>
      Table.rowsURI(PointTableParser)(base.resolve("/points.csv")).map(_.toList) shouldBe Success(points.map(Success(_)))
      Table.rowsURI(PointTableParser)(URI.create(s"$base/points.csv#row=3-4")).map(_.toList) shouldBe Success(points.slice(1, 3).map(Success(_)))

  it should "read the rows of a table from a jar" in:
    Table.rowsURI(PointTableParser)(LocalSources.jarURI("points.csv", csv)).map(_.toList) shouldBe Success(points.map(Success(_)))

object URLSourceSpec:
  case class Point(x: Int, y: Int)

  object Point extends CellParsers:
    given CellParser[Point] = CellParser.derived[Point]
    given StringParser[Point] = StandardRowParser.create[Point]

    given HeadedCSVTableParser[Point] = PointTableParser

  object PointTableParser extends HeadedCSVTableParser[Point]

  val csv: String = "x,y\n1,2\n3,4\n5,6\n7,8\n"

  val points: Seq[Point] = Seq(Point(1, 2), Point(3, 4), Point(5, 6), Point(7, 8))

  def file(): File =
    val f = File.createTempFile("points", ".csv")
    f.deleteOnExit()
    Files.writeString(f.toPath, csv)
    f
