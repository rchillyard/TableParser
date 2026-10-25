package com.phasmidsoftware.tableparser.cats.table

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.tableparser.core.parse.{CellParser, CellParsers, HeadedCSVTableParser, RawTableParser, StandardRowParser, StringParser, TableParser}
import com.phasmidsoftware.tableparser.core.render.CsvProduct
import com.phasmidsoftware.tableparser.core.table.Header
import fs2.Stream
import java.io.{ByteArrayInputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.io.Source

/**
 * Tests of streaming (with fs2) in IOTable: rows are parsed as they are pulled, so the whole table is never in memory.
 */
class IOTableStreamSpec extends AnyFlatSpec with should.Matchers:

  import IOTableStreamSpec._

  private def run[X](s: Stream[IO, X]): List[X] = s.compile.toList.unsafeRunSync()

  private def tempFile: File =
    val f = File.createTempFile("stream", ".csv")
    f.deleteOnExit()
    f

  behavior of "IOTable.stream"

  it should "stream rows of the type the parser makes" in:
    val ps: List[Point] = run(IOTable.streamLines(PointTableParser)(Seq("x,y", "1,2", "3,4")))
    ps shouldBe List(Point(1, 2), Point(3, 4))

  it should "read only as far as it needs to (here, from endless input)" in:
    val endless = "x,y" #:: LazyList.from(1).map(i => s"$i,${i * i}")
    run(IOTable.streamLines(PointTableParser)(endless).take(3)) shouldBe List(Point(1, 1), Point(2, 4), Point(3, 9))

  it should "fail when it reaches a row which can't be parsed (unless the parser is forgiving)" in:
    val lines = Seq("x,y", "1,2", "oops,3", "4,5")
    IOTable.streamLines(PointTableParser)(lines).compile.toList.attempt.unsafeRunSync() should matchPattern { case Left(_) => }
    run(IOTable.streamLines(PointTableParser)(lines).take(1)) shouldBe List(Point(1, 2))
    run(IOTable.streamLines(ForgivingPointTableParser)(lines)) shouldBe List(Point(1, 2), Point(4, 5))

  it should "select rows with the parser's predicate" in:
    val parser = RawTableParser.create(_.map(_.ws.head != "3").getOrElse(true), None)
    run(IOTable.streamLines(parser)(Seq("x,y", "1,2", "3,4", "5,6"))).map(_.ws) shouldBe List(Seq("1", "2"), Seq("5", "6"))

  it should "join a row whose quoted field spans lines" in:
    val parser = RawTableParser.create(TableParser.includeAll, None, multiline = true)
    run(IOTable.streamLines(parser)(Seq("a,b", "\"line 1", "line 2\",x", "y,z"))).map(_.ws) shouldBe List(Seq("line 1\nline 2", "x"), Seq("y", "z"))

  it should "use a fixed header (and read every line as a row)" in:
    val parser = RawTableParser.create(TableParser.includeAll, Some(Header.create("a", "b")))
    run(IOTable.streamLines(parser)(Seq("1,2", "3,4"))).map(_.ws) shouldBe List(Seq("1", "2"), Seq("3", "4"))

  it should "close its source, also when the stream stops early" in:
    val in = new ClosingStream("x,y\n1,2\n3,4\n")
    run(IOTable.stream(PointTableParser)(Source.fromInputStream(in)).take(1)) shouldBe List(Point(1, 2))
    in.closed shouldBe true

  it should "stream a resource" in:
    run(IOTable.streamResource(PointTableParser)("points.csv", classOf[IOTableStreamSpec])) shouldBe List(Point(1, 2), Point(3, 4))

  behavior of "IOTable pipes"

  it should "render rows as CSV lines" in:
    run(Stream.emits(points).through(IOTable.csvLines[Point])) shouldBe List("x,y", "1,2", "3,4", "5,6")

  it should "write rows to a CSV file, which can be streamed back" in:
    val file = tempFile
    Stream.emits(points).covary[IO].through(IOTable.writeCSVFile[Point](file.toPath)).compile.drain.unsafeRunSync()
    Files.readString(file.toPath) shouldBe "x,y\r\n1,2\r\n3,4\r\n5,6\r\n"
    run(IOTable.streamFile(PointTableParser)(file.toPath)) shouldBe points

object IOTableStreamSpec:
  case class Point(x: Int, y: Int) derives CsvProduct

  object Point extends CellParsers:
    given CellParser[Point] = CellParser.derived[Point]

    given StringParser[Point] = StandardRowParser.create[Point]

  object PointTableParser extends HeadedCSVTableParser[Point]

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
