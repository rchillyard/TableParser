package com.phasmidsoftware.parse

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

class LineParserSpec extends FlatSpec with Matchers {

  val hgTabbed = "Hello\tGoodbye"
  val hgSerial = "Hello, Goodbye"

  behavior of "LineParser"

  val p1 = new LineParser(", *".r, """\w+""".r, quote = '"')
  val p2 = new LineParser("""\t""".r, """\w+""".r, quote = ''')


  it should "cell" in {
    p1.parseAll(p1.cell, "Hello") should matchPattern { case p1.Success("Hello", _) => }
    p2.parseAll(p2.cell, "Hello") should matchPattern { case p2.Success("Hello", _) => }
  }

  it should "quotedString" in {
    p1.parseAll(p1.quotedString,""""Hello\tGoodbye"""") should matchPattern { case p1.Success("""Hello\tGoodbye""", _) => }
    p2.parseAll(p2.quotedString,"""'Hello,Goodbye'""") should matchPattern { case p2.Success("""Hello,Goodbye""", _) => }
  }

  it should "row" in {
    p1.parseAll(p1.row, hgSerial) should matchPattern { case p1.Success(Seq("Hello", "Goodbye"), _) => }
    p2.parseAll(p2.row, hgTabbed) should matchPattern { case p2.Success(Seq("Hello", "Goodbye"), _) => }
  }

  it should "parseRow" in {
    p1.parseRow(hgSerial) should matchPattern { case Success(Seq("Hello", "Goodbye")) => }
    p2.parseRow(hgTabbed) should matchPattern { case Success(Seq("Hello", "Goodbye")) => }
  }

}
