package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.render.{CsvProduct, CsvProductGenerator}
import com.phasmidsoftware.tableparser.core.table.Table
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
 * A case class may define vals in its body: parsing and rendering use only its parameters
 * (in 1.x, which found the fields by reflection, they had to be defs). See the README's Caveats.
 */
class CaseClassBodySpec extends AnyFlatSpec with should.Matchers:

  import CaseClassBodySpec._

  behavior of "a case class with vals in its body"

  it should "be parsed by its parameters" in:
    given HeadedCSVTableParser[Pair] = PairTableParser
    Table.parse[Table[Pair]](Seq("x,y", "2,3")).map(_.toSeq) shouldBe scala.util.Success(Seq(Pair(2, 3)))

  it should "be rendered by its parameters" in:
    summon[CsvProduct[Pair]].render(Pair(2, 3)) shouldBe "2,3"
    summon[CsvProduct[Pair]].asInstanceOf[CsvProductGenerator[Pair]].toColumnNames(None, None) shouldBe "x,y"

object CaseClassBodySpec:
  case class Pair(x: Int, y: Int) derives CsvProduct:
    val sum: Int = x + y
    lazy val product: Int = x * y

  object Pair extends CellParsers:
    given CellParser[Pair] = CellParser.derived[Pair]

    given StringParser[Pair] = StandardRowParser.create[Pair]

  object PairTableParser extends HeadedCSVTableParser[Pair]
