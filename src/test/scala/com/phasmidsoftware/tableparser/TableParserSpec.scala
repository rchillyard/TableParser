package com.phasmidsoftware.tableparser

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success, Try}
import scala.util.parsing.combinator.JavaTokenParsers

class TableParserSpec extends FlatSpec with Matchers {

  case class IntPair(a: Int, b: Int)

  class IntPairParser extends JavaTokenParsers {
    def pair: Parser[(Int,Int)] = wholeNumber ~ wholeNumber ^^ { case x ~ y => (x.toInt,y.toInt) }
  }

  val intPairParser = new IntPairParser

  trait IntPairRowParser extends RowParser[IntPair] {
    override def parse(w: String)(header: Seq[String]): Try[IntPair] = intPairParser.parseAll(intPairParser.pair,w) match {
      case intPairParser.Success((x,y),_) => Success(IntPair(x,y))
      case _ => Failure(TableException(s"unable to parse $w"))
    }

    override def parseHeader(w: String): Seq[String] = ???
  }

  implicit object IntPairRowParser extends IntPairRowParser

  trait IntPairTableParser extends TableParser[Table[IntPair]] {
    type Row = IntPair

    def hasHeader: Boolean = false

    def rowParser: RowParser[Row] = implicitly[RowParser[Row]]

    def builder(rows: Seq[Row]): Table[IntPair] = TableWithoutHeader(rows)
  }

  implicit object IntPairTableParser extends IntPairTableParser {
  }

  behavior of "TableParserSpec"

  it should "unit" in {

    val strings: Seq[String] = Seq("1 2")
    Table.parse(strings) match {
      case Success(t) => println(t);
      case Failure(x) => fail(x.getLocalizedMessage)
    }
  }

}
