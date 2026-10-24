package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.table.CsvAttributes
import org.scalatest.flatspec
import org.scalatest.matchers.should
import scala.util.Success

class RowConfigSpec extends flatspec.AnyFlatSpec with should.Matchers {

  private def parse(w: String)(using c: RowConfig): scala.util.Try[Strings] = LineParser.apply.parseRow(w -> 0)

  behavior of "RowConfig"

  it should "read RFC 4180 by default" in {
    val c = RowConfig.default
    c.delimiterRegex.regex shouldBe """\Q,\E"""
    c.string.regex shouldBe """[^,"]*"""
    parse("""a, b,"c,d"""")(using c) shouldBe Success(Seq("a", " b", "c,d"))
  }

  // Issue #48: changing the delimiter changes the string regex too.
  it should "derive the string regex from the delimiter" in {
    val c = RowConfig.default withDelimiter '\t'
    c.string.regex shouldBe "[^\t\"]*"
    parse("a b\tc,d\t\"e\tf\"")(using c) shouldBe Success(Seq("a b", "c,d", "e\tf"))
  }

  it should "escape a delimiter which is special in a regex" in {
    val c = RowConfig(delimiter = '|', listSep = ';')
    c.string.regex shouldBe """[^|"]*"""
    parse("a|b c|d")(using c) shouldBe Success(Seq("a", "b c", "d"))
    val d = RowConfig(delimiter = '^')
    parse("a^b")(using d) shouldBe Success(Seq("a", "b"))
  }

  it should "derive the string regex from the quote" in {
    val c = RowConfig.default withQuote '\''
    c.string.regex shouldBe """[^,']*"""
    parse("""'a,b',"c"""")(using c) shouldBe Success(Seq("a,b", "\"c\""))
  }

  it should "trim spaces next to the delimiter if asked" in {
    parse("a , b,c")(using RowConfig.trimmingRowConfig) shouldBe Success(Seq("a", "b", "c"))
    RowConfig.trimmingRowConfig shouldBe (RowConfig.default withTrim true)
  }

  it should "use an explicit string regex if given one" in {
    val c = RowConfig.default withString "[a-z]*".r
    c.string.regex shouldBe "[a-z]*"
    c.withDelimiter(';').string.regex shouldBe "[a-z]*"
  }

  it should "change one property at a time with the with methods" in {
    val c = RowConfig.default withDelimiter ';' withListSep ',' withListEnclosure ""
    c shouldBe RowConfig(delimiter = ';', listSep = ',', listEnclosure = "")
  }

  // Issue #38: reading and writing agree.
  it should "give the CsvAttributes which write what it reads" in {
    RowConfig.default.csvAttributes shouldBe CsvAttributes(",", "\"")
    RowConfig(delimiter = '\t', quote = '\'').csvAttributes shouldBe CsvAttributes("\t", "'")
    RowConfig.trimmingRowConfig.csvAttributes shouldBe CsvAttributes(", ", "\"")
  }

  it should "be made from the CsvAttributes whose output it reads" in {
    CsvAttributes(",").rowConfig shouldBe RowConfig.default
    CsvAttributes(", ").rowConfig shouldBe RowConfig.trimmingRowConfig
    CsvAttributes("\t", "'").rowConfig shouldBe RowConfig(delimiter = '\t', quote = '\'')
    an[IllegalArgumentException] should be thrownBy CsvAttributes("::").rowConfig
  }

  it should "round-trip between RowConfig and CsvAttributes" in {
    for (c <- Seq(RowConfig.default, RowConfig.trimmingRowConfig, RowConfig(delimiter = ';', quote = '\'')))
      c.csvAttributes.rowConfig shouldBe c
  }
}
