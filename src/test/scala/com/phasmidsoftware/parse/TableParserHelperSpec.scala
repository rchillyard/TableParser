/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table._
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat
import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.io.Codec
import scala.util.matching.Regex
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.{Failure, Success, Try}

/**
  * This tests one aspect of TableParserHelper which cannot be conveniently tested in the same module as the other.
  */
class TableParserHelperSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "TableParserHelper without a header row in the input file"

  case class Player(first: String, last: String) {
    def nickname: String = s"$first ${last.head}"
  }

  object Player extends TableParserHelper[Player](false) {
    def cellParser: CellParser[Player] = cellParser2(apply)
  }

  case class Partnership(playerA: String, playerB: String) {
    def asArray: Array[String] = Array(playerA, playerB)
  }

  object Partnership {
    def apply(players: Seq[Player]): Partnership = Partnership(players.head.nickname, players.last.nickname)
  }

  case class Partnerships(partners: Array[Array[String]]) {
    def size: Int = partners.length
  }

  it should "support fixed header" in {
    val strings = List("Adam,Sullivan", "Amy,Avergun", "Ann,Peterson", "Barbara,Goldman")
    val pty: Try[Table[Player]] = Table.parse[Table[Player]](strings.iterator)
    val pssy: Try[Iterator[Seq[Player]]] = for (pt <- pty) yield pt.rows grouped 2
    val tsy: Try[Iterator[Partnership]] = for (pss <- pssy) yield for (ps <- pss) yield Partnership(ps)
    val sy: Try[Partnerships] = for (ts <- tsy) yield Partnerships((for (t <- ts) yield t.asArray).toArray)
    sy should matchPattern { case Success(_) => }
    val partnerships: Partnerships = sy.get
    partnerships.size shouldBe 2
    partnerships.partners.head shouldBe Array("Adam S", "Amy A")
    partnerships.partners.last shouldBe Array("Ann P", "Barbara G")
  }
}
