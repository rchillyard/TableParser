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
  val p3 = new LineParser(", *".r, """[\w_\?:=\./]+""".r, "", '|', quote = '\'')
  //  val p4 = new LineParser("""\|""".r, """[^|]*""".r, "{}", ',', quote = '"')
  private val helloQuoteGoodbye = """"Hello ""Goodbye""""
  private val helloQuoteGoodbyeWithNewline =
    """"Hello
      |me old mate" "Goodbye"""".stripMargin
  private val HelloGoodbye = """Hello "Goodbye"""
  val HelloCommaGoodbye = """{Hello,Goodbye}"""

  it should "parse cell" in {
    p1.parseAll(p1.cell, "Hello") should matchPattern { case p1.Success("Hello", _) => }
    p2.parseAll(p2.cell, "Hello") should matchPattern { case p2.Success("Hello", _) => }
    p3.parseAll(p3.cell, "https://www.imdb.com/title/tt0499549/?ref_=fn_tt_tt_1") should matchPattern { case p3.Success("https://www.imdb.com/title/tt0499549/?ref_=fn_tt_tt_1", _) => }
    p3.parse(p3.cell, "https://www.imdb.com/title/tt5289954/?ref_=fn_tt_tt_1,,,,,,,12,7.1,,0") should matchPattern { case p3.Success("https://www.imdb.com/title/tt5289954/?ref_=fn_tt_tt_1", _) => }
    p1.parse(p1.cell, "https://www.imdb.com/title/tt5289954/?ref_=fn_tt_tt_1,,,,,,,12,7.1,,0") should matchPattern { case p1.Success("https://www.imdb.com/title/tt5289954/?ref_=fn_tt_tt_1", _) => }
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
    p1.parseAll(p1.quotedString, """"Hello\tGoodbye"""") should matchPattern { case p1.Success("""Hello\tGoodbye""", _) => }
    p2.parseAll(p2.quotedString, """'Hello,Goodbye'""") should matchPattern { case p2.Success("""Hello,Goodbye""", _) => }
    p1.parseAll(p1.quotedString, helloQuoteGoodbye) should matchPattern { case p1.Success(`HelloGoodbye`, _) => }
  }

  it should "parse quotedString with internal quotes" in {
    p1.parseAll(p1.quotedString, helloQuoteGoodbye) should matchPattern { case p1.Success(`HelloGoodbye`, _) => }
  }

  it should "fail to parse quotedString with internal newline" in {
    p1.parseAll(p1.quotedString, helloQuoteGoodbyeWithNewline) should matchPattern { case p1.Failure("end of input expected", _) => }
    p1.parseAll(p1.cell, helloQuoteGoodbyeWithNewline) should matchPattern { case p1.Failure("end of input expected", _) => }
  }

  it should "parse list" in {
    p1.parseAll(p1.list, HelloCommaGoodbye) should matchPattern { case p1.Success(`HelloCommaGoodbye`, _) => }
    p2.parseAll(p2.list, "Hello") should matchPattern { case p2.Failure(_, _) => }
    p2.parseAll(p2.list, """Hello|Goodbye""") should matchPattern { case p2.Success("{Hello,Goodbye}", _) => }
    p2.parseAll(p2.list, """Action|Adventure|Fantasy|Sci-Fi""") should matchPattern { case p2.Success("{Action,Adventure,Fantasy,Sci-Fi}", _) => }

  }

  it should "parse row" in {
    p1.parseAll(p1.row, hgSerial) should matchPattern { case p1.Success(Seq("Hello", "Goodbye"), _) => }
    p2.parseAll(p2.row, hgTabbed) should matchPattern { case p2.Success(Seq("Hello", "Goodbye"), _) => }
    p1.parseAll(p1.row, helloQuoteGoodbye) should matchPattern { case p1.Success(Seq(`HelloGoodbye`), _) => }
  }

  it should "parseRow" in {
    p1.parseRow((hgSerial, 0)) should matchPattern { case Success(Seq("Hello", "Goodbye")) => }
    p2.parseRow((hgTabbed, 0)) should matchPattern { case Success(Seq("Hello", "Goodbye")) => }
    p3.parseRow((hgSerialWithList, 0)) should matchPattern { case Success(Seq("Hello", "{Goodbye,From,Me}")) => }
    p1.parseRow((helloQuoteGoodbye, 0)) should matchPattern { case Success(Seq(`HelloGoodbye`)) => }
  }

  it should "parse problem situation" in {
    val w: String = "1|391F322E2DE3EAF7C9029DB2BB873C3B1E60FD1F657B97DB17031B8774A21EE45E2740DC65246C0FA712290AE8255406BDA708D166029E80F4B31236AC33A6D43A09370196D43191715E9817A9846D66DF7E159BDC641344AE7196AEAD9CC44FF7F8D2A33A3D153D7ADC8DBD3312381896BEAC462EF4DEB4C05F502DE312994EA9D679E3825593291C4CFEBFC653F3121DC3FDA2FCDB80E7C072D7EC95942BFAD9EFD7ACCF51BA38D96A4E3A325C860FFA47C94093751B58C4A9A257931876F2ADEEEFF4C3E4662339D5F3066CB625B9EB0E508AB6C4FD950CA259BCC6EC4283AB9521758B5E7D1CAE4BFE852B76BDD3F390C2C6CF65BB15FDB5B91CEB5F6D6AF4FCC8318AF911BDA27E5419225D3B5274D4AF5C75D0E1089F94"
    val rowConfig = RowConfig.defaultEncryptedRowConfig
    val lineParser: LineParser = LineParser.apply(rowConfig)
    val parseResult = lineParser.parseAll(lineParser.row, w)
    parseResult should matchPattern { case lineParser.Success(_, _) => }
  }

}
