/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.util.Success

class LineParserSpec extends flatspec.AnyFlatSpec with should.Matchers {

  val hgTabbed = "Hello\tGoodbye"
  val hgSerial = "Hello, Goodbye"
  val hgSerialWithList = "Hello, Goodbye|From|Me"
  val p1 = new LineParser(", *".r, """[^,]*""".r, "{}", ',', quote = '"')

  behavior of "LineParser"
  // TODO fix deprecation of syntax (next two lines).
  val p2 = new LineParser("""\t""".r, """[^\t]*""".r, "", '|', quote = '\'')
  val p3 = new LineParser(", *".r, """[\w_\?:=\.\/]+""".r, "", '|', quote = '\'')
  val p4 = new LineParser("|".r, """[^|]*""".r, "{}", ',', quote = '"')
  private val helloQuoteGoodbye = """"Hello ""Goodbye""""
  private val HelloGoodbye = """Hello "Goodbye"""
  val HelloCommaGoodbye = """{Hello,Goodbye}"""

  it should "parse cell" in {
    p1.parseAll(p1.cell, "Hello") should matchPattern { case p1.Success("Hello", _) => }
    p2.parseAll(p2.cell, "Hello") should matchPattern { case p2.Success("Hello", _) => }
    p3.parseAll(p3.cell, "http://www.imdb.com/title/tt0499549/?ref_=fn_tt_tt_1") should matchPattern { case p3.Success("http://www.imdb.com/title/tt0499549/?ref_=fn_tt_tt_1", _) => }
    p3.parse(p3.cell, "http://www.imdb.com/title/tt5289954/?ref_=fn_tt_tt_1,,,,,,,12,7.1,,0") should matchPattern { case p3.Success("http://www.imdb.com/title/tt5289954/?ref_=fn_tt_tt_1", _) => }
    p1.parse(p1.cell, "http://www.imdb.com/title/tt5289954/?ref_=fn_tt_tt_1,,,,,,,12,7.1,,0") should matchPattern { case p1.Success("http://www.imdb.com/title/tt5289954/?ref_=fn_tt_tt_1", _) => }
    p1.parseAll(p1.cell, helloQuoteGoodbye) should matchPattern { case p1.Success(`HelloGoodbye`, _) => }
  }

  it should "parse quotedStringWithQuotesAsList" in {
    p1.parseAll(p1.quotedStringWithQuotesAsList, helloQuoteGoodbye) should matchPattern { case p1.Success(Seq("Hello ", "Goodbye"), _) => }

  }
  it should "parse quotedStringWithQuotes" in {
    val result = p1.parseAll(p1.quotedStringWithQuotes, helloQuoteGoodbye)
    result should matchPattern { case p1.Success(_, _) => }
    result.get shouldBe HelloGoodbye

  }
  it should "parse quotedString" in {
    p1.parseAll(p1.quotedString,""""Hello\tGoodbye"""") should matchPattern { case p1.Success("""Hello\tGoodbye""", _) => }
    p2.parseAll(p2.quotedString,"""'Hello,Goodbye'""") should matchPattern { case p2.Success("""Hello,Goodbye""", _) => }
    p1.parseAll(p1.quotedString, helloQuoteGoodbye) should matchPattern { case p1.Success(`HelloGoodbye`, _) => }
  }

  it should "parse quotedString with internal quotes" in {
    p1.parseAll(p1.quotedString, helloQuoteGoodbye) should matchPattern { case p1.Success(`HelloGoodbye`, _) => }
  }

  it should "parse list" in {
    p1.parseAll(p1.list,HelloCommaGoodbye) should matchPattern { case p1.Success(`HelloCommaGoodbye`, _) => }
    p2.parseAll(p2.list, "Hello") should matchPattern { case p2.Failure(_, _) => }
    p2.parseAll(p2.list,"""Hello|Goodbye""") should matchPattern { case p2.Success("{Hello,Goodbye}", _) => }
    p2.parseAll(p2.list,"""Action|Adventure|Fantasy|Sci-Fi""") should matchPattern { case p2.Success("{Action,Adventure,Fantasy,Sci-Fi}", _) => }

  }

  it should "parse row" in {
    p1.parseAll(p1.row, hgSerial) should matchPattern { case p1.Success(Seq("Hello", "Goodbye"), _) => }
    p2.parseAll(p2.row, hgTabbed) should matchPattern { case p2.Success(Seq("Hello", "Goodbye"), _) => }
    p1.parseAll(p1.row, helloQuoteGoodbye) should matchPattern { case p1.Success(Seq(`HelloGoodbye`), _) => }
  }

  it should "parseRow" in {
    p1.parseRow(hgSerial) should matchPattern { case Success(Seq("Hello", "Goodbye")) => }
    p2.parseRow(hgTabbed) should matchPattern { case Success(Seq("Hello", "Goodbye")) => }
    p3.parseRow(hgSerialWithList) should matchPattern { case Success(Seq("Hello", "{Goodbye,From,Me}")) => }
    p1.parseRow(helloQuoteGoodbye) should matchPattern { case Success(Seq(`HelloGoodbye`)) => }
  }

}
