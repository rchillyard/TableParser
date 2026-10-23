/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.util.EvaluateTry.matchTry
import org.scalatest.flatspec
import org.scalatest.matchers.should
import scala.util.Try

/**
 * This tests one aspect of TableParserHelper which cannot be conveniently tested in the same module as the other.
 */
class TableParserHelperSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "TableParserHelper without a header row in the input file"

  case class Player(first: String, last: String) {
    def nickname: String = s"$first ${last.head}"
  }

  object Player extends TableParserHelper[Player](false) {
    lazy val cellParser: CellParser[Player] = CellParser.derived[Player]

    /**
     * Method to transform a Table[Player] into a Table[Partnership].
     *
     * The requirements of the application are that the rows of the Player table are grouped by twos
     * and each resulting entity (an array of length 2) is taken to form a Partnership.
     *
     * CONSIDER extracting a general method and moving it into Content.
     *
     * CONSIDER merging with duplicate code in: TableParserSpec.scala
     *
     * @param pt a Table[Player]
     * @return a Table[Partnership]
     */
    def convertTable(pt: Table[Player]): Table[Partnership] = pt.processRows(xs => Content((xs.toSeq grouped 2).map(r => Partnership(r)).toList))
  }

  case class Partnership(playerA: String, playerB: String) {
    def asArray: Array[String] = Array(playerA, playerB)
  }

  object Partnership {
    def apply(players: Iterable[Player]): Partnership = Partnership(players.head.nickname, players.last.nickname)
  }

  case class Partnerships(partners: Array[Array[String]]) {
    def size: Int = partners.length
  }

  it should "support fixed header" in {
    val strings = List("Adam,Sullivan", "Amy,Avagadro", "Ann,Peterson", "Barbara,Goldman")
    val pty: Try[Table[Player]] = Table.parse[Table[Player]](strings.iterator)
    matchTry(pty) {
      case pt@HeadedTable(_, _) =>
        val sht: Table[Partnership] = Player.convertTable(pt)
        val partnerships: Partnerships = Partnerships((for (t <- sht) yield t.asArray).toArray)
        partnerships.size shouldBe 2
        partnerships.partners.head shouldBe Array("Adam S", "Amy A")
        partnerships.partners.last shouldBe Array("Ann P", "Barbara G")
    }
  }
}
