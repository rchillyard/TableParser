package com.phasmidsoftware.tableparser.zio.table

import com.phasmidsoftware.tableparser.core.parse.{CellParser, CellParsers, HeadedCSVTableParser, RawTableParser, StandardRowParser, StringParser, TableParser}
import com.phasmidsoftware.tableparser.core.render.CsvProduct
import com.phasmidsoftware.tableparser.core.table.{Header, RawRow, Table}
import com.phasmidsoftware.tableparser.core.util.LocalSources
import com.phasmidsoftware.tableparser.zio.util.EvaluateZIO
import java.io.{ByteArrayInputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.io.Source
import zio.stream.ZStream

/**
 * Tests of streaming (with ZStream) in ZIOTable: rows are parsed as they are pulled, so the whole table is never in memory.
 */
class ZIOTableStreamSpec extends AnyFlatSpec with should.Matchers:

  import ZIOTableStreamSpec._

  private def run[X](s: ZStream[Any, Throwable, X]): List[X] = EvaluateZIO.run(s.runCollect).fold(e => throw e, _.toList)

  private def tempFile: File =
    val f = File.createTempFile("stream", ".csv")
    f.deleteOnExit()
    f

  behavior of "ZIOTable.stream"

  it should "stream rows of the type the parser makes" in:
    val ps: List[Point] = run(ZIOTable.streamLines(PointTableParser)(Seq("x,y", "1,2", "3,4")))
    ps shouldBe List(Point(1, 2), Point(3, 4))

  it should "read only as far as it needs to (here, from endless input)" in:
    val endless = "x,y" #:: LazyList.from(1).map(i => s"$i,${i * i}")
    run(ZIOTable.streamLines(PointTableParser)(endless).take(3)) shouldBe List(Point(1, 1), Point(2, 4), Point(3, 9))

  it should "fail when it reaches a row which can't be parsed (unless the parser is forgiving)" in:
    val lines = Seq("x,y", "1,2", "oops,3", "4,5")
    EvaluateZIO.run(ZIOTable.streamLines(PointTableParser)(lines).runCollect) should matchPattern { case Left(_) => }
    run(ZIOTable.streamLines(PointTableParser)(lines).take(1)) shouldBe List(Point(1, 2))
    run(ZIOTable.streamLines(ForgivingPointTableParser)(lines)) shouldBe List(Point(1, 2), Point(4, 5))

  it should "leave out rows with the parser's raw predicate, before they are parsed" in:
    val lines = Seq("x,y", "1,2", "oops,3", "4,5")
    run(ZIOTable.streamLines(RawFilteredPointTableParser)(lines)) shouldBe List(Point(1, 2), Point(4, 5))

  it should "select rows with the parser's predicate" in:
    val parser = RawTableParser.create(_.map(_.ws.head != "3").getOrElse(true), None)
    run(ZIOTable.streamLines(parser)(Seq("x,y", "1,2", "3,4", "5,6"))).map(_.ws) shouldBe List(Seq("1", "2"), Seq("5", "6"))

  it should "join a row whose quoted field spans lines" in:
    val parser = RawTableParser.create(TableParser.includeAll, None, multiline = true)
    run(ZIOTable.streamLines(parser)(Seq("a,b", "\"line 1", "line 2\",x", "y,z"))).map(_.ws) shouldBe List(Seq("line 1\nline 2", "x"), Seq("y", "z"))

  it should "use a fixed header (and read every line as a row)" in:
    val parser = RawTableParser.create(TableParser.includeAll, Some(Header.create("a", "b")))
    run(ZIOTable.streamLines(parser)(Seq("1,2", "3,4"))).map(_.ws) shouldBe List(Seq("1", "2"), Seq("3", "4"))

  it should "close its source, also when the stream stops early" in:
    val in = new ClosingStream("x,y\n1,2\n3,4\n")
    run(ZIOTable.stream(PointTableParser)(Source.fromInputStream(in)).take(1)) shouldBe List(Point(1, 2))
    in.closed shouldBe true

  it should "stream only the rows selected by a URI's fragment (RFC 7111)" in:
    val file = tempFile
    Files.writeString(file.toPath, "x,y\n1,2\n3,4\n5,6\n7,8\n")
    run(ZIOTable.streamURI(PointTableParser)(new java.net.URI(s"${file.toURI}#row=3-4"))) shouldBe List(Point(3, 4), Point(5, 6))
    run(ZIOTable.streamURI(PointTableParser)(file.toURI)).size shouldBe 4

  it should "stream a table over HTTP, and from a jar (#128: up to 2.5.0, only file: URIs)" in:
    val csv = "x,y\n1,2\n3,4\n5,6\n"
    LocalSources.withHttpServer(Map("/points.csv" -> csv)): base =>
      run(ZIOTable.streamURI(PointTableParser)(base.resolve("/points.csv"))) shouldBe points
      run(ZIOTable.streamURI(PointTableParser)(java.net.URI.create(s"$base/points.csv#row=3-4"))) shouldBe List(Point(3, 4), Point(5, 6))
      val uri = base.resolve("/points.csv")
      given TableParser[Table[Point]] = PointTableParser
      EvaluateZIO.run(ZIOTable.parse[Table[Point]](uri)).map(_.toSeq) shouldBe Right(points)
    run(ZIOTable.streamURI(PointTableParser)(LocalSources.jarURI("points.csv", csv))) shouldBe points

  it should "stream a resource" in:
    run(ZIOTable.streamResource(PointTableParser)("points.csv", classOf[ZIOTableStreamSpec])) shouldBe List(Point(1, 2), Point(3, 4))

  behavior of "ZIOTable pipelines and sinks"

  it should "render rows as CSV lines" in:
    run(ZStream.fromIterable(points).via(ZIOTable.csvLines[Point])) shouldBe List("x,y", "1,2", "3,4", "5,6")

  it should "write rows to a CSV file, which can be streamed back" in:
    val file = tempFile
    EvaluateZIO.run(ZStream.fromIterable(points).run(ZIOTable.writeCSVFile[Point](file.toPath))) should matchPattern { case Right(_) => }
    Files.readString(file.toPath) shouldBe "x,y\r\n1,2\r\n3,4\r\n5,6\r\n"
    run(ZIOTable.streamFile(PointTableParser)(file.toPath)) shouldBe points

  it should "emit the greatest and least rows of a stream" in:
    given Ordering[Point] = Ordering.by(_.y)
    run(ZStream.fromIterable(points).via(ZIOTable.topN[Point](2))) shouldBe List(Point(5, 6), Point(3, 4))
    run(ZStream.fromIterable(points).via(ZIOTable.bottomN[Point](2))) shouldBe List(Point(1, 2), Point(3, 4))
    run(ZStream.empty.via(ZIOTable.topN[Point](2))) shouldBe Nil

  it should "emit the greatest rows of a stream which is parsed as it goes" in:
    given Ordering[Point] = Ordering.by(_.y)
    val lines = "x,y" #:: LazyList.range(1, 100001).map(i => s"$i,${i % 1000}")
    run(ZIOTable.streamLines(PointTableParser)(lines).via(ZIOTable.topN[Point](3))).map(_.y) shouldBe List(999, 999, 999)

object ZIOTableStreamSpec:
  case class Point(x: Int, y: Int) derives CsvProduct

  object Point extends CellParsers:
    given CellParser[Point] = CellParser.derived[Point]

    given StringParser[Point] = StandardRowParser.create[Point]

  object PointTableParser extends HeadedCSVTableParser[Point]

  // NOTE a parser which extends HeadedCSVTableParser sets a raw predicate by overriding maybeRawPredicate.
  object RawFilteredPointTableParser extends HeadedCSVTableParser[Point]:
    override protected val maybeRawPredicate: Option[RawRow => Boolean] = Some(r => r("x") != scala.util.Success("oops"))

  object ForgivingPointTableParser extends HeadedCSVTableParser[Point]:
    override val forgiving: Boolean = true

  val points: List[Point] = List(Point(1, 2), Point(3, 4), Point(5, 6))

  /**
   * An InputStream which remembers whether it has been closed.
   */
  class ClosingStream(w: String) extends ByteArrayInputStream(w.getBytes(StandardCharsets.UTF_8)):
    var closed = false

    override def close(): Unit =
      closed = true
      super.close()
