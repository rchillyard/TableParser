/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.table

import com.phasmidsoftware.parse.{RowParser, StringParser, StringTableParser}
import com.phasmidsoftware.render._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}

class TableSpec extends FlatSpec with Matchers {

  case class IntPair(a: Int, b: Int) {
    def map(f: Int => Int): IntPair = IntPair(f(a), f(b))
  }

  object IntPair {

    class IntPairParser extends JavaTokenParsers {
      def pair: Parser[(Int, Int)] = wholeNumber ~ wholeNumber ^^ { case x ~ y => (x.toInt, y.toInt) }
    }

    val intPairParser = new IntPairParser

    trait IntPairRowParser extends StringParser[IntPair] {
      override def parse(w: String)(header: Header): Try[IntPair] = intPairParser.parseAll(intPairParser.pair, w) match {
        case intPairParser.Success((x, y), _) => Success(IntPair(x, y))
        case _ => Failure(TableException(s"unable to parse $w"))
      }

      //noinspection NotImplementedCode
      override def parseHeader(w: String): Try[Header] = ???
    }

    implicit object IntPairRowParser extends IntPairRowParser

    trait IntPairTableParser extends StringTableParser[Table[IntPair]] {
      type Row = IntPair

      val maybeHeader: Option[Header] = Some(Header.create("a", "b"))


      def builder(rows: Seq[IntPair], header: Header): Table[IntPair] = TableWithHeader(rows, Header[IntPair]())

      def rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]
    }

    implicit object IntPairTableParser extends IntPairTableParser

  }

  behavior of "Table"

  it should "parse from Seq[String]" in {
    import IntPair._
    val iIty = Table.parse(Seq("1 2", "42 99"))
    iIty should matchPattern { case Success(_) => }
    iIty.get.size shouldBe 2
  }

  it should "parse from Iterator[String]" in {
    import IntPair._
    val iIty = Table.parse(Seq("1 2", "42 99").iterator)
    iIty should matchPattern { case Success(_) => }
    iIty.get.size shouldBe 2
  }

  it should "parse from Source" in {
    import IntPair._

    val source = Source.fromChars(Array('1', ' ', '2', '\n', '4', '2', ' ', '9', '9', '\n'))
    val iIty = Table.parse(source)
    iIty should matchPattern { case Success(_) => }
    iIty.get.size shouldBe 2
  }

  it should "parse from Resource" in {
    import IntPair._

    val iIty = Table.parseResource("intPairs.csv", classOf[TableSpec])
    iIty should matchPattern { case Success(_) => }
    iIty.get.size shouldBe 2
  }

  it should "do iterator" in {
    import IntPair._
    val iIty = Table.parse(Seq("1 2", "42 99"))
    iIty should matchPattern { case Success(_) => }
    val x = iIty.get.iterator
    x.hasNext shouldBe true
    x.next() shouldBe IntPair(1, 2)
    x.hasNext shouldBe true
    x.next() shouldBe IntPair(42, 99)
    x.hasNext shouldBe false
  }

  it should "map" in {
    val f: IntPair => IntPair = _ map (_ * 2)

    import IntPair._
    val iIty = Table.parse(Seq("1 2", "42 99"))
    iIty should matchPattern { case Success(_) => }
    iIty.get.map(f).rows shouldBe Seq(IntPair(2, 4), IntPair(84, 198))
  }

  it should "flatMap" in {
    val f: IntPair => Table[IntPair] = p => TableWithHeader(Seq(p), Header())

    import IntPair._
    val iIty = Table.parse(Seq("1 2", "42 99"))
    iIty should matchPattern { case Success(_) => }
    iIty.get.flatMap(f).rows shouldBe Seq(IntPair(1, 2), IntPair(42, 99))
  }

  case class HTML(x: String, ao: Option[String], attr: Map[String, String], hs: Seq[HTML])

  object HTML {
    def apply(x: String): HTML = apply(x, None, Map.empty, Nil)

    def apply(x: String, a: String): HTML = apply(x, Some(a), Map.empty, Nil)

    def apply(x: String, hs: Seq[HTML]): HTML = apply(x, None, Map.empty, hs)

  }

  object IntPairHTML extends Renderers {

    trait HTMLTreeWriter extends TreeWriter[HTML] {
      def evaluate(node: Node): HTML = HTML(node.style, node.content map (_.toString), node.attributes, node.children map evaluate)
    }

    implicit object HTMLTreeWriter extends HTMLTreeWriter

    implicit val intPairRenderer: Renderer[IntPair] = renderer2("IntPair")(IntPair.apply)
    implicit val r: Renderer[Indexed[IntPair]] = indexedRenderer("", "th")

  }

  it should "render the parsed table to CSV" in {
    import IntPair._
    val iIty: Try[Table[IntPair]] = Table.parse(Seq("1 2", "42 99"))
    iIty should matchPattern { case Success(_) => }

    implicit object StringBuilderWriteable extends Writable[StringBuilder] {
      override def unit: StringBuilder = new StringBuilder

      override def delimiter: CharSequence = "|"

      override def writeRaw(o: StringBuilder)(x: CharSequence): StringBuilder = o.append(x.toString)
    }

    val sy = iIty map (_.render)
    sy should matchPattern { case Success(_) => }
    sy.get.toString shouldBe "a|b\n1|2\n42|99\n"
  }

  it should "render the parsed table with TreeWriter" in {
    import IntPair._
    val iIty = Table.parse(Seq("1 2", "42 99"))
    import IntPairHTML._
    val hy = iIty map (_.render("table", Map()))
    hy should matchPattern { case Success(_) => }
    // CONSIDER why do we use ArrayBuffer here instead of List?
    hy.get shouldBe HTML("table", None, Map(), List(HTML("thead", None, Map(), List(HTML("tr", None, Map(), ArrayBuffer(HTML("th", Some("a"), Map(), List()), HTML("th", Some("b"), Map(), List()))))), HTML("tbody", None, Map(), List(HTML("IntPair", None, Map(), List(HTML("", Some("1"), Map("name" -> "a"), List()), HTML("", Some("2"), Map("name" -> "b"), List()))), HTML("IntPair", None, Map(), List(HTML("", Some("42"), Map("name" -> "a"), List()), HTML("", Some("99"), Map("name" -> "b"), List())))))))
  }


}
