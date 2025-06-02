/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.cats.table

import cats.effect.IO
import com.phasmidsoftware.tableparser.cats.util.EvaluateIO
import com.phasmidsoftware.tableparser.cats.util.EvaluateIO.matchIO
import com.phasmidsoftware.tableparser.core.parse._
import com.phasmidsoftware.tableparser.core.render._
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.write.{Node, TreeWriter, Writable}
import java.io.{File, FileWriter, InputStream}
import org.scalatest.flatspec
import org.scalatest.matchers.should
import scala.annotation.unused
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}

class TableSpec extends flatspec.AnyFlatSpec with should.Matchers {

  case class IntPair(a: Int, b: Int) {
    def map(f: Int => Int): IntPair = IntPair(f(a), f(b))
  }

  object IntPair {

    object IntPairParser extends JavaTokenParsers {
      lazy val pair: Parser[(Int, Int)] = wholeNumber ~ wholeNumber ^^ { case x ~ y => (x.toInt, y.toInt) }
    }

    trait IntPairRowParser extends StringParser[IntPair] {
      def parse(indexedString: (String, Int))(header: Header): Try[IntPair] = IntPairParser.parseAll(IntPairParser.pair, indexedString._1) match {
        case IntPairParser.Success((x, y), _) => Success(IntPair(x, y))
        case _ => Failure(TableException(s"unable to parse ${indexedString._1}"))
      }

      //noinspection NotImplementedCode
      def parseHeader(w: Seq[String]): Try[Header] = ???
    }

    implicit object IntPairRowParser extends IntPairRowParser

    trait IntPairTableParser extends StringTableParser[Table[IntPair]] {
      type Row = IntPair

      val maybeFixedHeader: Option[Header] = Some(Header.create("a", "b"))

      val headerRowsToRead: Int = 0

      protected def builder(rows: Iterable[IntPair], header: Header): Table[IntPair] = HeadedTable(rows, Header[IntPair]())

      val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]
    }

    implicit object IntPairTableParser extends IntPairTableParser

  }

  behavior of "Table"

  it should "parse table from file" in {
    import IntPair._
    val z1 = IO.fromTry(Table.parseFile("core/src/test/resources/com/phasmidsoftware/tableparser/core/table/intPairs.csv", "UTF-8"))
    val z2 = IO.fromTry(Table.parseFile("core/src/test/resources/com/phasmidsoftware/tableparser/core/table/intPairs.csv"))
    matchIO(z1 product z2) {
      case (a@HeadedTable(_, _), b@HeadedTable(_, _)) => a.size shouldBe 2; b.size shouldBe 2
    }
  }

  // NOTE: this test can be flaky. Perhaps we should just use zip instead of parProduct.
  it should "parse table from raw file" in {
    val z1: IO[Table[RawRow]] = IO.fromTry(Table.parseFileRaw(new File("output.csv"), TableParser.includeAll, Some(Header(Seq(Seq("a", "b"))))))
    val z2: IO[Table[RawRow]] = IO.fromTry(Table.parseFileRaw("core/src/test/resources/com/phasmidsoftware/tableparser/core/table/intPairs.csv", TableParser.includeAll))
    matchIO(z1 product z2) {
      case (a@HeadedTable(_, _), b@HeadedTable(_, _)) =>
        a.size shouldBe 0; b.size shouldBe 1
    }
  }

  it should "write table to the file" in {
    val hdr = Header(Seq(Seq("a", "b")))
    val row1 = Row(Seq("1", "2"), hdr, 1)
    val table = Table(Seq(row1), Some(hdr))
    val resultIO = for {_ <- IOTable.writeCSVFileRow(table, new File("output.csv"))
                        _ = println(s"written to file output.csv")
                        y <- IO.fromTry(Table.parseFileRaw("output.csv", TableParser.includeAll))
                        } yield y
    matchIO(resultIO) {
      case xt@HeadedTable(_, _) => xt.content.head.toString() shouldBe """A="1", B="2""""
    }
    val tableWithoutHead = Table(Seq(row1), None)
    the[TableException] thrownBy Table.writeCSVFileRow(tableWithoutHead, new File("output.csv"))
  }

  it should "parse from Iterator[String]" in {
    import IntPair._
    matchIO(IO.fromTry(Table.parse(Seq("1 2", "42 99").iterator))) {
      case xt@HeadedTable(_, _) => xt.size shouldBe 2
    }
  }

  behavior of "parse with safeResource"

  it should "return failure(1)" in {
    import IntPair._

    lazy val si: IO[InputStream] = IO(classOf[TableSpec].getResourceAsStream(null))
    val iIty = for (s <- si) yield Table.parseInputStream(s)
    import cats.effect.unsafe.implicits.global
    // TODO eliminate use of unsafe methods
    EvaluateIO.checkFailure(iIty)(classOf[NullPointerException]).unsafeRunSync()
  }

  behavior of "other"

  it should "map" in {
    val f: IntPair => IntPair = _ map (_ * 2)

    import IntPair._
    matchIO(IOTable.parse(Seq("1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.map(f).content.toSeq shouldBe Seq(IntPair(2, 4), IntPair(84, 198))
    }
  }

  it should "flatMap" in {
    val f: IntPair => Table[IntPair] = p => HeadedTable(Seq(p), Header())

    import IntPair._
    matchIO(IOTable.parse(Seq("1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.flatMap(f).content.toSeq shouldBe Seq(IntPair(1, 2), IntPair(42, 99))
    }
  }

  it should "to Seq" in {
    import IntPair._
    matchIO(IOTable.parse(Seq("1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.toSeq shouldBe Seq(IntPair(1, 2), IntPair(42, 99))
    }
  }

  it should "to Shuffle" in {
    import IntPair._
    matchIO(IOTable.parse(Seq("1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.shuffle.content.size shouldBe 2
    }
  }

  it should "drop" in {
    import IntPair._
    matchIO(IOTable.parse(Seq("1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.drop(1).content.toSeq shouldBe Seq(IntPair(42, 99))
    }
  }

  it should "empty" in {
    import IntPair._
    matchIO(IOTable.parse(Seq("1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.empty.content.toSeq shouldBe Seq.empty
    }
  }

  it should "dropWhile" in {
    import IntPair._
    matchIO(IOTable.parse(Seq("3 4", "1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.dropWhile(_.equals(IntPair(3, 4))).content.toSeq shouldBe Seq(IntPair(1, 2), IntPair(42, 99))
    }
  }

  it should "filter" in {
    import IntPair._
    matchIO(IOTable.parse(Seq("3 4", "1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.filter(_.equals(IntPair(3, 4))).content.toSeq shouldBe Seq(IntPair(3, 4))
    }
  }

  it should "filterNot" in {
    import IntPair._
    matchIO(IOTable.parse(Seq("3 4", "1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.filterNot(_.equals(IntPair(3, 4))).content.toSeq shouldBe Seq(IntPair(1, 2), IntPair(42, 99))
    }
  }

  it should "slice" in {
    import IntPair._
    matchIO(IOTable.parse(Seq("3 4", "1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.slice(0, 2).content.toSeq shouldBe Seq(IntPair(3, 4), IntPair(1, 2))
    }
  }

  it should "takeWhile" in {
    import IntPair._
    matchIO(IOTable.parse(Seq("3 4", "1 2", "42 99"))) {
      case xt@HeadedTable(_, _) =>
        xt.takeWhile(_.equals(IntPair(3, 4))).content.toSeq shouldBe Seq(IntPair(3, 4))
    }
  }

  case class HTML(x: String, ao: Option[String], attr: Map[String, String], hs: Seq[HTML])

  object HTML {
    def apply(x: String): HTML = apply(x, None, Map.empty, Nil)

    def apply(x: String, a: String): HTML = apply(x, Some(a), Map.empty, Nil)

    def apply(x: String, hs: Seq[HTML]): HTML = apply(x, None, Map.empty, hs)
  }

  object IntPairHTML extends HierarchicalRenderers {

    trait HTMLTreeWriter extends TreeWriter[HTML] {
      def evaluate(node: Node): HTML = HTML(node.style, node.content map identity, node.attributes, node.children map evaluate)
    }

    implicit object HTMLTreeWriter extends HTMLTreeWriter

    implicit val intPairRenderer: HierarchicalRenderer[IntPair] = renderer2("IntPair")(IntPair.apply)
    implicit val r: HierarchicalRenderer[Indexed[IntPair]] = indexedRenderer("", "th")
  }

  // TODO this is a mystery: it sometimes fails (?)
  it should "render the table to CSV" in {
    import IntPair._
    matchIO(IOTable.parse(Seq("1 2", "42 99"))) {
      case HeadedTable(_, _) => succeed
      case x => fail(s"error: $x")
    }

    implicit object StringBuilderWritable extends Writable[StringBuilder] {
      def unit: StringBuilder = new StringBuilder

      override def delimiter: CharSequence = "|"

      def writeRaw(o: StringBuilder)(x: CharSequence): StringBuilder = o.append(x.toString)
    }

    implicit object DummyRenderer$$ extends Renderer[Table[IntPair], String] {
      def render(t: Table[IntPair], attrs: Map[String, String]): String =
        t match {
          case t: RenderableTable[IntPair] => t.renderToWritable(StringBuilderWritable).toString
          case _ => throw TableException("render problem")
        }
    }

    val wi: IO[String] = IOTable.parse(Seq("1 2", "42 99")) map {
      case r: Table[IntPair] => implicitly[Renderer[Table[IntPair], String]].render(r)
      case _ => fail("cannot render table")
    }
    matchIO(wi) {
      case "a|b\n1|2\n42|99\n" => succeed
      case x => fail(s"string is $x")
    }
  }

  it should "render the table to CSV using a Writable" in {
    import IntPair._
    matchIO(IOTable.parse(Seq("1 2", "42 99"))) {
      case HeadedTable(_, _) => succeed
    }

    val file = new File("output.csv")
    implicit val fw: Writable[FileWriter] = Writable.fileWritable(file)

    implicit object FileRenderer extends Renderer[Table[IntPair], FileWriter] {
      def render(t: Table[IntPair], attrs: Map[String, String]): FileWriter = t match {
        case pr: RenderableTable[IntPair] => pr.renderToWritable
        case _ => throw TableException("render problem")
      }
    }

    //    implicit object FileCsvRenderer extends CsvRenderer[Table[IntPair]] {
    //      def render(t: Table[IntPair], attrs: Map[String, String]): FileWriter = t match {
    //        case pr: RenderableTable[IntPair] => pr.renderToWritable
    //        case _ => throw TableException("render problem")
    //      }
    //    }

    val fi: IO[FileWriter] = IOTable.parse(Seq("1 2", "42 99")) map {
      case r: Table[IntPair] =>
        val z: Renderer[Table[IntPair], FileWriter] = implicitly[Renderer[Table[IntPair], FileWriter]]
        z.render(r)
      case _ => fail("cannot render table")
    }
    matchIO(fi) {
      case _ => succeed
    }
  }

  it should "render another parsed table to CSV" in {
    import IntPair._

    implicit object IntPairCsvRenderer extends CsvRenderer[IntPair] {
      val csvAttributes: CsvAttributes = CsvAttributes(", ")

      def render(t: IntPair, attrs: Map[String, String]): String = s"${t.a}${csvAttributes.delimiter}${t.b}"
    }

    implicit object IntPairCsvGenerator extends CsvProductGenerator[IntPair] {
      val csvAttributes: CsvAttributes = CsvAttributes(", ")

      def toColumnNames(po: Option[String], no: Option[String]): String = s"a${csvAttributes.delimiter}b"
    }

    implicit val csvAttributes: CsvAttributes = IntPairCsvRenderer.csvAttributes
    matchIO(IOTable.parseFile(new File("core/src/test/resources/com/phasmidsoftware/tableparser/core/table/intPairs.csv"))) {
      case iIt@HeadedTable(_, _) =>
        val ws = IO.fromTry(iIt.toCSV)
        EvaluateIO(ws) shouldBe "a, b\n1, 2\n42, 99\n"
    }
  }

  it should "render another parsed table to CSV with delim, quote" in {
    import IntPair._
    implicit val myCsvAttributes: CsvAttributes = CsvAttributes("|")

    implicit object IntPairCsvRenderer extends CsvRenderer[IntPair] {
      val csvAttributes: CsvAttributes = myCsvAttributes

      def render(t: IntPair, attrs: Map[String, String]): String = s"${t.a}${csvAttributes.delimiter}${t.b}"
    }

    implicit object IntPairCsvGenerator extends CsvProductGenerator[IntPair] {
      val csvAttributes: CsvAttributes = myCsvAttributes

      def toColumnNames(wo: Option[String], no: Option[String]): String = s"a${csvAttributes.delimiter}b"
    }

    matchIO(IOTable.parseFile(new File("core/src/test/resources/com/phasmidsoftware/tableparser/core/table/intPairs.csv"))) {
      case iIt@HeadedTable(_, _) =>
        val ws = IO.fromTry(iIt.toCSV)
        EvaluateIO(ws) shouldBe "a|b\n1|2\n42|99\n"
    }
  }

  it should "render the parsed table with TreeWriter" in {
    import IntPair._
    @unused
    val iIty: IO[Table[IntPair]] = IO.fromTry(Table.parse(Seq("1 2", "42 99")))
    // TODO restore ...
//      val hy = iIty map {
//        case r: HeadedTable[IntPair] => r.render
//      }
//      hy should matchPattern { case Success(_) => }
//      // CONSIDER why do we use ArrayBuffer here instead of List?
//      hy.get shouldBe HTML("table", None, Map(), List(HTML("thead", None, Map(), List(HTML("tr", None, Map(), Seq(HTML("th", Some("a"), Map(), List()), HTML("th", Some("b"), Map(), List()))))), HTML("tbody", None, Map(), List(HTML("IntPair", None, Map(), List(HTML("", Some("1"), Map("name" -> "a"), List()), HTML("", Some("2"), Map("name" -> "b"), List()))), HTML("IntPair", None, Map(), List(HTML("", Some("42"), Map("name" -> "a"), List()), HTML("", Some("99"), Map("name" -> "b"), List())))))))
  }

  def mapTo[T, U](ty: Try[T]): Try[U] = ty match {
    case Success(t) => Success(t.asInstanceOf[U])
    case Failure(x) => Failure(x)
  }


  behavior of "sort"

  it should "sort a Table and then select" in {
    import IntPair._
    matchIO(IOTable.parse(Seq("1 2", "42 99", "1 3"))) {
      case mt: Table[IntPair] =>

        implicit object IntPairOrdering extends Ordering[IntPair] {
          def compare(x: IntPair, y: IntPair): Int = x.a.compareTo(y.a) match {
            case 0 => x.b.compareTo(y.b)
            case cf => cf
          }
        }
        val x: Table[IntPair] = mt.sort
        val row1 = x.select(Range(2, 3))
        row1.size shouldBe 1
        row1.head shouldBe IntPair(1, 3)
    }
  }

  behavior of "Header"
  it should "do lookup" in {
    val hdr = Header(Seq(Seq("a", "Hello Goodbye", "Team Number")))
    hdr.getIndex("a") shouldBe Success(0)
    hdr.getIndex("Hello Goodbye") shouldBe Success(1)
    hdr.getIndex("team number") shouldBe Success(2)
  }

  behavior of "Table[Row]"
  it should "work" in {
    val hdr = Header(Seq(Seq("a", "b")))
    val row1 = Row(Seq("1", "2"), hdr, 1)
    val table = Table(Seq(row1), Some(hdr))
    EvaluateIO(IO.fromTry(Table.toCSVRow(table))) shouldBe "a,b\n1,2\n"
  }
}
