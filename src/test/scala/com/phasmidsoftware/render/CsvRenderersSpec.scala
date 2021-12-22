package com.phasmidsoftware.render

import com.phasmidsoftware.parse.{RowParser, StringParser, StringTableParser}
import com.phasmidsoftware.table._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.io.File
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}

class CsvRenderersSpec extends AnyFlatSpec with should.Matchers {

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
      def parseHeader(w: String): Try[Header] = ???
    }

    implicit object IntPairRowParser extends IntPairRowParser

    trait IntPairTableParser extends StringTableParser[Table[IntPair]] {
      type Row = IntPair

      val maybeFixedHeader: Option[Header] = Some(Header.create("a", "b"))


      protected def builder(rows: Iterable[IntPair], header: Header): Table[IntPair] = HeadedTable(rows, Header[IntPair]())

      val rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]
    }

    implicit object IntPairTableParser extends IntPairTableParser

  }

  behavior of "CsvRenderers"

  it should "render an Option[Int]" in {
    import CsvRenderers._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    implicit val optionIntRenderer: CsvRenderer[Option[Int]] = new CsvRenderers {}.optionRenderer
    optionIntRenderer.render(Some(42)) shouldBe "42"
    optionIntRenderer.render(None) shouldBe ""
  }

  it should "render an 1-tuple" in {
    import CsvRenderers._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    case class Onesy(x: Int)
    implicit val onesyCsvRenderer: CsvRenderer[Onesy] = new CsvRenderers {}.renderer1(Onesy)
    onesyCsvRenderer.render(Onesy(42)) shouldBe "42"
  }

  it should "render an IntPair" in {
    import CsvRenderers._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    implicit val intPairCsvRenderer: CsvRenderer[IntPair] = new CsvRenderers {}.renderer2(IntPair.apply)
    val intPair = IntPair(42, 99)
    intPairCsvRenderer.render(intPair) shouldBe "42, 99"
  }

  behavior of "CsvTableRenderer"

  it should "render a table" in {
    val csvRenderers = new CsvRenderers {}
    import CsvRenderers._
    implicit val csvAttributes: CsvAttributes = CsvAttributes(", ")
    implicit val intPairCsvRenderer: CsvRenderer[IntPair] = csvRenderers.renderer2(IntPair.apply)
    import IntPair._
    val iIty = Table.parseFile(new File("src/test/resources/com/phasmidsoftware/table/intPairs.csv"))
    iIty should matchPattern { case Success(_) => }
    val iIt = iIty.get
    val ws = CsvTableRenderer[IntPair]().render(iIt)
    ws.head shouldBe "a, b"
    ws.tail.head shouldBe "1, 2"
    ws.tail.tail.head shouldBe "42, 99"
  }
}
