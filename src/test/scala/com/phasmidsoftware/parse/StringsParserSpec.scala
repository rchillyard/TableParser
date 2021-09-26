/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.Header
import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.util.{Success, Try}

class StringsParserSpec extends flatspec.AnyFlatSpec with should.Matchers {

  case class HawkCount(species: String, count: Int)

  object HawkCountParser extends CellParsers {
    val header: Header = Header.create("species", "count") // TODO do we need this?

    implicit val hawkCountColumnHelper: ColumnHelper[HawkCount] = columnHelper()
    implicit val hawkCountParser: CellParser[HawkCount] = cellParser2(HawkCount)
  }

  behavior of "RowParser"

  it should "parse regex string" in {
    import HawkCountParser._

    val parser = StandardStringsParser[HawkCount]()
    val hawkCount: Try[HawkCount] = parser.parse((Seq("Red-tailed Hawk", "1027"), 0))(header)
    hawkCount shouldBe Success(HawkCount("Red-tailed Hawk", 1027))
  }

  it should "parse quoted string" in {
    import HawkCountParser._
    val parser = StandardStringsParser[HawkCount]()
    val hawkCount: Try[HawkCount] = parser.parse((Seq(""""Red-tailed Hawk"""", "1027"), 0))(header)
    hawkCount shouldBe Success(HawkCount(""""Red-tailed Hawk"""", 1027))
  }

}
