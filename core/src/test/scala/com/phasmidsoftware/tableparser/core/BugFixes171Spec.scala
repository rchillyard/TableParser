package com.phasmidsoftware.tableparser.core

import com.phasmidsoftware.tableparser.core.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.tableparser.core.render.{CsvGenerator, CsvGenerators, CsvRenderer, CsvRenderers}
import com.phasmidsoftware.tableparser.core.table.{CsvAttributes, Header, RawTable, Table}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Success

/**
 * The bugs fixed in 1.7.1 (back-ported from 2.0).
 */
class BugFixes171Spec extends AnyFlatSpec with Matchers {

  behavior of "writing a String (1.7.1)"

  it should "quote a String when it must be quoted to be read back (as in RFC 4180)" in {
    val renderer = implicitly[CsvRenderer[String]]
    renderer.render("plain") shouldBe "plain"
    renderer.render("a, comma") shouldBe "\"a, comma\""
    renderer.render("a \"quote\"") shouldBe "\"a \"\"quote\"\"\""
    renderer.render("a line\nbreak") shouldBe "\"a line\nbreak\""
    renderer.render(" spaces ") shouldBe "\" spaces \""
    renderer.render("") shouldBe ""
  }

  it should "quote a column name when it must be quoted" in {
    CsvRenderers.quoteIfNecessary("rating, 2026")(CsvAttributes(",")) shouldBe "\"rating, 2026\""
    implicitly[CsvGenerator[Int]].toColumnName(None, "rating, 2026") shouldBe "\"rating, 2026\""
  }

  behavior of "reading in multiline mode (1.7.1)"

  it should "keep the line break inside a quoted field" in {
    implicit val parser: RawTableParser = RawTableParser(TableParser.includeAll, None).setMultiline(true)
    Table.parse[RawTable](Seq("a,b", "\"x", "1\",y")).map(_.toSeq.map(_.ws)) shouldBe Success(Seq(Seq("x\n1", "y")))
  }

  behavior of "Header.getIndex (1.7.1)"

  it should "prefer a column whose name matches exactly, and otherwise ignore case" in {
    val header = Header.create("Name", "name", "Rating")
    header.getIndex("name") shouldBe Success(1)
    header.getIndex("Name") shouldBe Success(0)
    header.getIndex("rating") shouldBe Success(2)
  }

  behavior of "optionProductRenderer and optionProductGenerator (1.7.1)"

  it should "render a missing multi-column value as the right number of empty cells" in {
    val renderers = new CsvRenderers {}
    val generators = new CsvGenerators {}
    implicit val pairRenderer: CsvRenderer[Pair171] = renderers.renderer2(Pair171.apply)
    implicit val pairGenerator: CsvGenerator[Pair171] = generators.generator2(Pair171.apply)
    val optionRenderer = renderers.optionProductRenderer[Pair171]
    optionRenderer.render(Some(Pair171(1, 2))) shouldBe "1,2"
    optionRenderer.render(None) shouldBe ","
    generators.optionProductGenerator[Pair171].toColumnName(None, "pair") shouldBe "pair.a,pair.b"
  }
}

case class Pair171(a: Int, b: Int)
