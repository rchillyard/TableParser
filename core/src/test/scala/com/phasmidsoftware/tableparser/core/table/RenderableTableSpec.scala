package com.phasmidsoftware.tableparser.core.table

import com.phasmidsoftware.tableparser.core.write.Writable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
 * Tests of RenderableTable, HeadedTable and UnheadedTable which the other specs don't reach.
 */
class RenderableTableSpec extends AnyFlatSpec with should.Matchers:

  import RenderableTableSpec._

  private given Writable[StringBuilder] = Writable.stringBuilderWritable()

  behavior of "renderToWritable"

  it should "render rows which are case classes" in:
    val w = HeadedTable(Seq(Point(1, 2), Point(3, 4)), Header.create("x", "y")).renderToWritable[StringBuilder].toString
    w should include("x,y")
    w should include("1,2")
    w should include("3,4")

  // Up to 2.0.1, a List row matched Product first, and was written as its head and tail ("1,List(2)").
  it should "render rows which are sequences" in:
    val w = HeadedTable(Seq(Seq(1, 2), Seq(3, 4)), Header.create("x", "y")).renderToWritable[StringBuilder].toString
    w should include("1,2")
    w should include("3,4")

  it should "render rows which are arrays" in:
    val w = new UnheadedTable(Content(Seq(Array(1, 2), Array(3, 4)))).renderToWritable[StringBuilder].toString
    w should include("1,2")
    w should include("3,4")

  it should "fail to render rows which are none of these" in:
    a[TableException] should be thrownBy new UnheadedTable(Content(Seq(1, 2))).renderToWritable[StringBuilder]

  behavior of "HeadedTable and UnheadedTable"

  it should "be made from an Iterable or an Iterator, with or without a header" in:
    HeadedTable(Seq(Point(1, 2)).iterator, Header.create("x", "y")).size shouldBe 1
    HeadedTable(Seq(Point(1, 2))).maybeHeader shouldBe Some(Header.create("x", "y"))
    HeadedTable(Seq(Point(1, 2)).iterator).maybeHeader shouldBe Some(Header.create("x", "y"))
    UnheadedTable(Seq(Point(1, 2)).iterator).size shouldBe 1
    UnheadedTable(Seq(Point(1, 2))).maybeHeader shouldBe None

  it should "get a column of a raw table, or of a table of sequences" in:
    val header = Header.create("x", "y")
    val raw = HeadedTable(Seq(RawRow(Seq("1", "2"), header), RawRow(Seq("3", "4"), header)), header)
    raw.column("y").toList shouldBe List(Some("2"), Some("4"))
    val seqs = HeadedTable(Seq(Seq(1, 2), Seq(3, 4)), header)
    seqs.column("x").toList shouldBe List(Some("1"), Some("3"))
    HeadedTable(Seq(Point(1, 2)), header).column("x").toList shouldBe List(None)
    UnheadedTable(Seq(Seq(1, 2))).column("x").toList shouldBe Nil

  it should "describe itself" in:
    HeadedTable(Seq(Point(1, 2)), Header.create("x", "y")).toString should startWith("HeadedTable(")
    UnheadedTable(Seq(Point(1, 2))).toString should include("1 rows")

object RenderableTableSpec:
  case class Point(x: Int, y: Int)
