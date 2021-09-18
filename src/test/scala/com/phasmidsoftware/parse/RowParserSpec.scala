/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.Header
import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.util.matching.Regex
import scala.util.{Success, Try}

class RowParserSpec extends flatspec.AnyFlatSpec with should.Matchers {

  case class HawkCount(species: String, count: Int)

  object HawkCountParser extends CellParsers {
    val header: Header = Header.create("species", "count") // TODO do we need this?

    implicit val hawkCountParser: CellParser[HawkCount] = cellParser2(HawkCount)
  }

  behavior of "RowParser"

  it should "parse regex string" in {
    import HawkCountParser._
    trait HawkCountRowConfig extends DefaultRowConfig {
      override val string: Regex = """[a-zA-Z0-9 -]*""".r
    }
    implicit object HawkCountRowConfig extends HawkCountRowConfig

    val parser = StandardRowParser[HawkCount](LineParser.apply)
    val hawkCount: Try[HawkCount] = parser.parse(("""Red-tailed Hawk,1027""", 0))(header)
    hawkCount shouldBe Success(HawkCount("Red-tailed Hawk", 1027))
  }

  it should "parse quoted string" in {
    import HawkCountParser._
    val parser = StandardRowParser[HawkCount](LineParser.apply)
    val hawkCount: Try[HawkCount] = parser.parse((""""Red-tailed Hawk",1027""", 0))(header)
    hawkCount shouldBe Success(HawkCount("Red-tailed Hawk", 1027))
  }

}
