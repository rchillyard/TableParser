package com.phasmidsoftware.parse

import com.phasmidsoftware.table.Header
import org.scalatest.{FlatSpec, Matchers}

import scala.util.matching.Regex
import scala.util.{Success, Try}

class RowParserSpec extends FlatSpec with Matchers {

  case class HawkCount(species: String, count: Int)

  val header: Header = Header.create("species", "count")

  object HawkCountFormat extends Formats {

    implicit val hawkCountColumnHelper: ColumnHelper[HawkCount] = columnHelper()
    implicit val hawkCountFormat: CellParser[HawkCount] = cellReader2(HawkCount)
  }

  behavior of "RowParser"

  it should "parse regex string" in {
    import HawkCountFormat._
    trait HawkCountRowConfig extends DefaultRowConfig {
      override val string: Regex = """[a-zA-Z0-9 -]*""".r
    }
    implicit object HawkCountRowConfig extends HawkCountRowConfig

    val parser = StandardRowParser[HawkCount](LineParser.apply)
    val hawkCount: Try[HawkCount] = parser.parse("""Red-tailed Hawk,1027""")(header)
    hawkCount shouldBe Success(HawkCount("Red-tailed Hawk", 1027))
  }

  it should "parse quoted string" in {
    import HawkCountFormat._
    val parser = StandardRowParser[HawkCount](LineParser.apply)
    val hawkCount: Try[HawkCount] = parser.parse(""""Red-tailed Hawk",1027""")(header)
    hawkCount shouldBe Success(HawkCount("Red-tailed Hawk", 1027))
  }

}
