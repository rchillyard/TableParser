/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.Header
import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Success, Try}

class StringsParserSpec extends FlatSpec with Matchers {

	case class HawkCount(species: String, count: Int)

	val header: Header = Header.create("species", "count")

	object HawkCountParser extends CellParsers {

		implicit val hawkCountColumnHelper: ColumnHelper[HawkCount] = columnHelper()
		implicit val hawkCountParser: CellParser[HawkCount] = cellParser2(HawkCount)
	}

	behavior of "RowParser"

	it should "parse regex string" in {
		import HawkCountParser._

		val parser = StandardStringsParser[HawkCount]()
		val hawkCount: Try[HawkCount] = parser.parse(Seq("Red-tailed Hawk", "1027"))(header)
		hawkCount shouldBe Success(HawkCount("Red-tailed Hawk", 1027))
	}

	it should "parse quoted string" in {
		import HawkCountParser._
		val parser = StandardStringsParser[HawkCount]()
		val hawkCount: Try[HawkCount] = parser.parse(Seq(""""Red-tailed Hawk"""", "1027"))(header)
		hawkCount shouldBe Success(HawkCount(""""Red-tailed Hawk"""", 1027))
	}

}
