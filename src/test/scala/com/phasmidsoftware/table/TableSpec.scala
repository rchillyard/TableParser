/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.table

import com.phasmidsoftware.parse.{RowParser, StringParser, StringTableParser}
import org.scalatest.{FlatSpec, Matchers}

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

      def hasHeader: Boolean = false

      def rowParser: RowParser[Row, String] = implicitly[RowParser[Row, String]]

      def builder(rows: Seq[Row]): Table[IntPair] = TableWithoutHeader(rows)
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

  it should "$plus$plus" in {
    // TODO implement this test
  }

  it should "flatMap" in {
    val f: IntPair => Table[IntPair] = p => TableWithoutHeader(Seq(p))

    import IntPair._
    val iIty = Table.parse(Seq("1 2", "42 99"))
    iIty should matchPattern { case Success(_) => }
    iIty.get.flatMap(f).rows shouldBe Seq(IntPair(1, 2), IntPair(42, 99))
  }

}
