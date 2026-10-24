/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.table.Header
import org.scalatest.flatspec
import org.scalatest.matchers.should
import scala.util.{Success, Try}

class RowParserSpec extends flatspec.AnyFlatSpec with should.Matchers {

  case class HawkCount(species: String, count: Int)

  object HawkCountParser extends CellParsers {
    val header: Header = Header.create("species", "count") // CONSIDER do we need this?

    implicit val hawkCountParser: CellParser[HawkCount] = CellParser.derived[HawkCount]
  }

  behavior of "RowParser"

  it should "parse regex string" in {
    import HawkCountParser._
    implicit val hawkCountRowConfig: RowConfig = RowConfig.default withString """[a-zA-Z0-9 -]*""".r

    val parser = StandardRowParser[HawkCount](LineParser.apply)
    val hawkCount: Try[HawkCount] = parser.parseIndexed(header)(("""Red-tailed Hawk,1027""", 0))
    hawkCount shouldBe Success(HawkCount("Red-tailed Hawk", 1027))
  }

  it should "parse quoted string" in {
    import HawkCountParser._
    val parser = StandardRowParser[HawkCount](LineParser.apply)
    val hawkCount: Try[HawkCount] = parser.parseIndexed(header)((""""Red-tailed Hawk",1027""", 0))
    hawkCount shouldBe Success(HawkCount("Red-tailed Hawk", 1027))
  }

}
