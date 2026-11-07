package com.phasmidsoftware.tableparser.core.table

import com.phasmidsoftware.tableparser.core.parse.{CellParser, CellParsers, HeadedCSVTableParser, StandardRowParser, StringParser}
import java.io.{ByteArrayInputStream, File}
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.io.Source
import scala.util.{Success, Using}

/**
 * Tests of Table.rows and its variants (#119): the rows of a table, parsed as they are read, without an effect library.
 */
class RowIteratorSpec extends AnyFlatSpec with should.Matchers:

  import RowIteratorSpec._

  private def tempFile: File =
    val f = File.createTempFile("rows", ".csv")
    f.deleteOnExit()
    f

  behavior of "Table.rows"

  it should "give the header and the rows, of the type the parser makes" in:
    val result = for
      rows <- Table.rowsLines(PointTableParser)(Seq("x,y", "1,2", "3,4"))
      ps <- Using(rows)(rs => (rs.header.xs, rs.toList))
    yield ps
    result shouldBe Success((Seq("x", "y"), List(Success(Point(1, 2)), Success(Point(3, 4)))))

  it should "read only as far as it needs to (here, from endless input)" in:
    val endless = "x,y" #:: LazyList.from(1).map(i => s"$i,${i * i}")
    Table.rowsLines(PointTableParser)(endless).map(_.take(3).toList) shouldBe Success(List(Success(Point(1, 1)), Success(Point(2, 4)), Success(Point(3, 9))))

  it should "give a Failure for a row which can't be parsed (unless the parser is forgiving, which leaves it out)" in:
    val lines = Seq("x,y", "1,2", "3,oops", "4,5")
    Table.rowsLines(PointTableParser)(lines).map(_.map(_.isSuccess).toList) shouldBe Success(List(true, false, true))
    Table.rowsLines(ForgivingPointTableParser)(lines).map(_.toList) shouldBe Success(List(Success(Point(1, 2)), Success(Point(4, 5))))

  it should "close its source when it is exhausted" in:
    val in = new ClosingStream("x,y\n1,2\n")
    val rows = Table.rows(PointTableParser)(Source.fromInputStream(in)).get
    in.closed shouldBe false
    rows.toList shouldBe List(Success(Point(1, 2)))
    in.closed shouldBe true
    rows.hasNext shouldBe false

  it should "close its source when it is closed early, as with Using" in:
    val in = new ClosingStream("x,y\n1,2\n3,4\n5,6\n")
    Table.rows(PointTableParser)(Source.fromInputStream(in)).flatMap(rows => Using(rows)(_.next())) shouldBe Success(Success(Point(1, 2)))
    in.closed shouldBe true

  it should "close its source, and fail, if the header can't be read" in:
    val in = new ClosingStream("\"x,y\n1,2\n")
    Table.rows(PointTableParser)(Source.fromInputStream(in)).isFailure shouldBe true
    in.closed shouldBe true

  it should "read a file" in:
    val file = tempFile
    Files.writeString(file.toPath, "x,y\n1,2\n3,4\n")
    Table.rowsFile(PointTableParser)(file.toPath).map(_.toList) shouldBe Success(List(Success(Point(1, 2)), Success(Point(3, 4))))

  it should "read only the rows selected by a URI's fragment (RFC 7111)" in:
    val file = tempFile
    Files.writeString(file.toPath, "x,y\n1,2\n3,4\n5,6\n7,8\n")
    Table.rowsURI(PointTableParser)(new java.net.URI(s"${file.toURI}#row=3-4")).map(_.toList) shouldBe Success(List(Success(Point(3, 4)), Success(Point(5, 6))))
    Table.rowsURI(PointTableParser)(file.toURI).map(_.size) shouldBe Success(4)

  it should "read a resource" in:
    Table.rowsResource(PointTableParser)("points.csv", classOf[RowIteratorSpec]).map(_.toList) shouldBe Success(List(Success(Point(1, 2)), Success(Point(3, 4))))

  it should "fail for a resource which doesn't exist" in:
    Table.rowsResource(PointTableParser)("nowhere.csv", classOf[RowIteratorSpec]).isFailure shouldBe true

  behavior of "Table.parseRow"

  it should "parse a row with its header line" in:
    Table.parseRow(PointTableParser)(Some("x,y"), "1,2") shouldBe Success(Some(Point(1, 2)))
    Table.parseRow(PointTableParser)(Some("y,x"), "1,2") shouldBe Success(Some(Point(2, 1)))

  it should "use the parser's fixed header when there is no header line (and fail without one)" in:
    Table.parseRow(FixedHeaderPointTableParser)(None, "1,2") shouldBe Success(Some(Point(1, 2)))
    Table.parseRow(PointTableParser)(None, "1,2").isFailure shouldBe true

  it should "give None for a row which a forgiving parser leaves out, and a Failure otherwise" in:
    Table.parseRow(ForgivingPointTableParser)(Some("x,y"), "3,oops") shouldBe Success(None)
    Table.parseRow(PointTableParser)(Some("x,y"), "3,oops").isFailure shouldBe true

  it should "give None for a row which the raw predicate leaves out" in:
    Table.parseRow(RawFilteredPointTableParser)(Some("x,y"), "0,2") shouldBe Success(None)
    Table.parseRow(RawFilteredPointTableParser)(Some("x,y"), "1,2") shouldBe Success(Some(Point(1, 2)))

object RowIteratorSpec:
  case class Point(x: Int, y: Int)

  object Point extends CellParsers:
    given CellParser[Point] = CellParser.derived[Point]

    given StringParser[Point] = StandardRowParser.create[Point]

  object PointTableParser extends HeadedCSVTableParser[Point]

  object ForgivingPointTableParser extends HeadedCSVTableParser[Point]:
    override val forgiving: Boolean = true

  object FixedHeaderPointTableParser extends HeadedCSVTableParser[Point]:
    override protected val maybeHeader: Option[Header] = Some(Header.create("x", "y"))

  // NOTE a parser which extends HeadedCSVTableParser sets a raw predicate by overriding maybeRawPredicate.
  object RawFilteredPointTableParser extends HeadedCSVTableParser[Point]:
    override protected val maybeRawPredicate: Option[RawRow => Boolean] = Some(r => r("x") != Success("0"))

  /**
   * An InputStream which remembers whether it has been closed.
   */
  class ClosingStream(w: String) extends ByteArrayInputStream(w.getBytes(StandardCharsets.UTF_8)):
    var closed = false

    override def close(): Unit =
      closed = true
      super.close()
