package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.table.{CsvFragment, Header, RawRow, Row, RowSelection}
import java.util.concurrent.atomic.AtomicInteger
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success, Try}

/**
 * Tests of the raw predicate (setRawPredicate), which selects rows before they are parsed (issue #109).
 */
class RawPredicateSpec extends AnyFlatSpec with should.Matchers:

  import RawPredicateSpec._

  private val lines = Seq("x,y", "1,2", "3,4", "5,6")

  behavior of "setRawPredicate"

  it should "parse only the rows whose raw cells satisfy it (by column name)" in:
    val (parser, conversions) = countingParser()
    parser.setRawPredicate(r => r("x") == Success("3")).parse(lines.iterator).map(_.toSeq) shouldBe Success(Seq(Point(3, 4)))
    conversions.get shouldBe 1

  it should "leave out a row which couldn't be converted, so that the parse doesn't fail" in:
    val (parser, _) = countingParser()
    parser.parse(Iterator("x,y", "1,2", "oops,3", "5,6")) should matchPattern { case Failure(_) => }
    parser.setRawPredicate(r => r("x") != Success("oops")).parse(Iterator("x,y", "1,2", "oops,3", "5,6")).map(_.toSeq) shouldBe Success(Seq(Point(1, 2), Point(5, 6)))

  it should "be applied before the predicate, which still applies to the parsed rows" in:
    val (parser, conversions) = countingParser()
    val p = parser.setPredicate(_.map(_.y > 3).getOrElse(false)).setRawPredicate(r => r("x") != Success("5"))
    p.parse(lines.iterator).map(_.toSeq) shouldBe Success(Seq(Point(3, 4)))
    conversions.get shouldBe 2

  it should "work with a RawTableParser, and with multiline rows" in:
    val parser = RawTableParser(multiline = true).setRawPredicate(r => r("a").toOption.exists(_.startsWith("line")))
    parser.parse(Iterator("a,b", "\"line 1", "line 2\",x", "y,z")).map(_.toSeq.map(_.ws)) shouldBe Success(Seq(Seq("line 1\nline 2", "x")))

  it should "work with a StandardStringsParser" in:
    val header = Header.create("x", "y")
    val parser = StandardStringsParser[Point]()
    parser.parseIndexedIf(header, Some(_("x") == Success("1")))(Seq("1", "2") -> 0) shouldBe Success(Some(Point(1, 2)))
    parser.parseIndexedIf(header, Some(_("x") == Success("1")))(Seq("3", "4") -> 1) shouldBe Success(None)
    parser.parseIndexedIf(header, None)(Seq("3", "4") -> 1) shouldBe Success(Some(Point(3, 4)))

  it should "fail clearly with a RowParser which can't apply it" in:
    val result = CustomRowParserTableParser.parse(lines.iterator)
    result should matchPattern { case Failure(ParserException(msg, _)) if msg.contains("can't apply a raw predicate") => }

  behavior of "the setters of PlainTextHeadedStringTableParser"

  it should "keep each other's settings" in:
    val p = PlainTextHeadedStringTableParser[Note]().setMultiline(true).setPredicate(_.map(_.id > 1).getOrElse(false)).setRawPredicate(r => r("id") != Success("3")).setForgiving(true)
    val input = Iterator("id,text", "1,a", "2,\"line 1", "line 2\"", "oops,b", "3,c", "4,d")
    // 1 fails the predicate, 3 the raw predicate; "oops" can't be parsed (forgiving); 2 is on two lines (multiline).
    p.parse(input).map(_.toSeq) shouldBe Success(Seq(Note(2, "line 1\nline 2"), Note(4, "d")))

  behavior of "rawSampler"

  it should "choose about one row in n, parsing only those" in:
    val (parser, conversions) = countingParser()
    val many = "x,y" +: (1 to 10000).map(i => s"$i,$i")
    val sampled = parser.setRawPredicate(TableParser.rawSampler(10)).parse(many.iterator).get
    sampled.size should (be > 800 and be < 1200)
    conversions.get shouldBe sampled.size

  behavior of "rows, with an RFC 7111 selection"

  it should "parse only the selected rows" in:
    val (parser, conversions) = countingParser()
    val selection = CsvFragment.parse("row=3").collect { case s: RowSelection => s }.get
    parser.rows(lines.iterator, selection = selection).map(_._2.toList) shouldBe Success(List(Success(Point(3, 4))))
    conversions.get shouldBe 1

  it should "number the rows as the input does, whatever the raw predicate leaves out" in:
    val parser = RawTableParser().setRawPredicate(r => r("x") != Success("1"))
    val selection = CsvFragment.parse("row=2-3").collect { case s: RowSelection => s }.get
    parser.rows(lines.iterator, selection = selection).map(_._2.map(_.map(_.ws)).toList) shouldBe Success(List(Success(Seq("3", "4"))))

  it should "work with rawSampler (the sample is of the selected rows)" in:
    val parser = RawTableParser().setRawPredicate(TableParser.rawSampler(1))
    val selection = CsvFragment.parse("row=2-3").collect { case s: RowSelection => s }.get
    parser.rows(lines.iterator, selection = selection).map(_._2.map(_.map(_.ws)).toList) shouldBe Success(List(Success(Seq("1", "2")), Success(Seq("3", "4"))))

object RawPredicateSpec:
  case class Point(x: Int, y: Int)

  object Point extends CellParsers:
    given CellParser[Point] = CellParser.derived[Point]

  case class Note(id: Int, text: String)

  object Note extends CellParsers:
    given CellParser[Note] = CellParser.derived[Note]

  /**
   * @return a parser of Points whose CellParser counts its conversions, and the count.
   */
  def countingParser(): (PlainTextHeadedStringTableParser[Point], AtomicInteger) =
    val conversions = new AtomicInteger(0)
    val derived: CellParser[Point] = Point.given_CellParser_Point
    given CellParser[Point] = new CellParser[Point]:
      def parse(wo: Option[String], row: Row, columns: Header): Try[Point] =
        conversions.incrementAndGet()
        derived.parse(wo, row, columns)

      def convertString(w: String): Try[Point] = derived.convertString(w)
    (PlainTextHeadedStringTableParser[Point](), conversions)

  // A RowParser which doesn't split rows into cells (so it can't apply a raw predicate).
  object CustomRowParser extends StringParser[Point]:
    def parse(header: Header)(input: String): Try[Point] = parseIndexed(header)(input -> 0)

    def parseIndexed(header: Header)(indexedString: (String, Int)): Try[Point] =
      Try(indexedString._1.split(",").map(_.toInt)).map(xs => Point(xs(0), xs(1)))

    def parseHeader(xs: Seq[String]): Try[Header] = Success(Header.create(xs.head.split(",").toSeq *))

  object CustomRowParserTableParser extends HeadedCSVTableParser[Point](using CustomRowParser):
    override protected val maybeRawPredicate: Option[RawRow => Boolean] = Some(_ => true)
