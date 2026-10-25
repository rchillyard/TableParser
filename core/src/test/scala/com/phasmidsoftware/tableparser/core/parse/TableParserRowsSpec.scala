package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.table.Header
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success}

/**
 * Tests of StringTableParser.rows, which parses lazily (the basis of streaming in the cats and zio modules).
 */
class TableParserRowsSpec extends AnyFlatSpec with should.Matchers:

  import TableParserRowsSpec._

  behavior of "StringTableParser.rows"

  it should "read the header, and parse each row only when it is pulled" in:
    var linesRead = 0
    val lines = ("x,y" #:: LazyList.from(1).map(i => s"$i,${i * i}")).iterator.map { w => linesRead += 1; w }
    val (header, rows) = PointTableParser.rows(lines).get
    header shouldBe Header.create("x", "y")
    linesRead shouldBe 1
    rows.take(2).toList shouldBe List(Success(Point(1, 1)), Success(Point(2, 4)))
    linesRead should be <= 4

  it should "leave a bad row as a Failure, unless the parser is forgiving" in:
    val lines = Seq("x,y", "1,2", "oops,3", "4,5")
    PointTableParser.rows(lines.iterator).get._2.toList should matchPattern { case List(Success(_), Failure(_), Success(_)) => }
    ForgivingPointTableParser.rows(lines.iterator).get._2.toList shouldBe List(Success(Point(1, 2)), Success(Point(4, 5)))

  it should "fail if there is no header and no header lines" in:
    PointTableParser.rows(Iterator("1,2"), 0) should matchPattern { case Failure(_: TableParserException) => }

object TableParserRowsSpec:
  case class Point(x: Int, y: Int)

  object Point extends CellParsers:
    given CellParser[Point] = CellParser.derived[Point]

    given StringParser[Point] = StandardRowParser.create[Point]

  object PointTableParser extends HeadedCSVTableParser[Point]

  object ForgivingPointTableParser extends HeadedCSVTableParser[Point]:
    override val forgiving: Boolean = true
