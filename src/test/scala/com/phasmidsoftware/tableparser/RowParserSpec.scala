package com.phasmidsoftware.tableparser

import com.phasmidsoftware.format.Formats
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Success, Try}

class RowParserSpec extends FlatSpec with Matchers {

  case class HawkCount(species: String, count: Int)

  val header: Seq[String] = Seq("species", "count")

  object HawkCountFormat extends Formats {

    import Formats._

    implicit val hawkCountFormat: CellParser[HawkCount] = cellReader2(HawkCount)
  }

  behavior of "RowParser"

  it should "parse regex string" in {
    import HawkCountFormat._
    val parser = StandardRowParser[HawkCount](string = """[a-zA-Z0-9 -]*""".r)
    val hawkCount: Try[HawkCount] = parser.parse("""Red-tailed Hawk,1027""")(header)
    hawkCount shouldBe Success(HawkCount("Red-tailed Hawk", 1027))
  }

  it should "parse quoted string" in {
    import HawkCountFormat._
    val parser = StandardRowParser[HawkCount]()
    val hawkCount: Try[HawkCount] = parser.parse(""""Red-tailed Hawk",1027""")(header)
    hawkCount shouldBe Success(HawkCount("Red-tailed Hawk", 1027))
  }

}
