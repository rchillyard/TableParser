package com.phasmidsoftware.tableparser

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}
import scala.util.parsing.combinator.JavaTokenParsers

class TableSpec extends FlatSpec with Matchers {

  case class IntPair(a: Int, b: Int) {
    def map(f: Int=>Int): IntPair = IntPair(f(a), f(b))
  }

  object IntPair {

    class IntPairParser extends JavaTokenParsers {
      def pair: Parser[(Int, Int)] = wholeNumber ~ wholeNumber ^^ { case x ~ y => (x.toInt, y.toInt) }
    }

    val intPairParser = new IntPairParser

    trait IntPairRowParser extends RowParser[IntPair] {
      override def parse(w: String)(header: Seq[String]): Try[IntPair] = intPairParser.parseAll(intPairParser.pair, w) match {
        case intPairParser.Success((x, y), _) => Success(IntPair(x, y))
        case _ => Failure(TableException(s"unable to parse $w"))
      }

      //noinspection NotImplementedCode
      override def parseHeader(w: String): Try[Seq[String]] = ???
    }

    implicit object IntPairRowParser extends IntPairRowParser

    trait IntPairTableParser extends TableParser[Table[IntPair]] {
      type Row = IntPair

      def hasHeader: Boolean = false

      def rowParser: RowParser[Row] = implicitly[RowParser[Row]]

      def builder(rows: Seq[Row]): Table[IntPair] = TableWithoutHeader(rows)
    }

    implicit object IntPairTableParser extends IntPairTableParser
  }

  behavior of "Table"

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
    val f: IntPair=>IntPair = _ map (_ * 2)

    import IntPair._
    val iIty = Table.parse(Seq("1 2", "42 99"))
    iIty should matchPattern { case Success(_) => }
    iIty.get.map(f).rows shouldBe Seq(IntPair(2,4), IntPair(84,198))
  }

  it should "$plus$plus" in {
    // TODO implement this test
  }

  it should "flatMap" in {
    val f: IntPair=>Table[IntPair] = p => TableWithoutHeader(Seq(p))

    import IntPair._
    val iIty = Table.parse(Seq("1 2", "42 99"))
    iIty should matchPattern { case Success(_) => }
    iIty.get.flatMap(f).rows shouldBe Seq(IntPair(1,2), IntPair(42,99))
  }

}
