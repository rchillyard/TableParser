/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import cats.effect.IO
import com.phasmidsoftware.render.{JsonTableRenderer, Renderer}
import com.phasmidsoftware.table._
import com.phasmidsoftware.util.EvaluateIO.matchIO
import org.scalatest.flatspec
import org.scalatest.matchers.should
import spray.json.{DefaultJsonProtocol, RootJsonFormat, enrichAny}

/**
 * This tests one aspect of TableParserHelper which cannot be conveniently tested in the same module as the other.
 */
class TableParserHelperSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "TableParserHelper without a header row in the input file"

  case class Player(first: String, last: String) {
    def nickname: String = s"$first ${last.head}"
  }

  object Player extends TableParserHelper[Player](false) {
    lazy val cellParser: CellParser[Player] = cellParser2(apply)

    /**
     * Method to transform a Table[Player] into a Table[Partnership].
     *
     * The requirements of the application are that the rows of the Player table are grouped by twos
     * and each resulting entity (an array of length 2) is taken to form a Partnership.
     *
     * @param pt a Table[Player]
     * @return a Table[Partnership]
     */
    def convertTable(pt: Table[Player]): Table[Partnership] = pt.processRows(xs => (xs grouped 2).toList).map(r => Partnership(r))
  }

  case class Partnership(playerA: String, playerB: String) {
    def asArray: Array[String] = Array(playerA, playerB)
  }

  object Partnership extends DefaultJsonProtocol {
    implicit val partnershipFormat: RootJsonFormat[Partnership] = jsonFormat2(apply)

    def apply(players: Iterable[Player]): Partnership = Partnership(players.head.nickname, players.last.nickname)
  }

  case class Partnerships(partners: Array[Array[String]]) {
    def size: Int = partners.length

    /**
     * Method to output these Partnerships as a Json String.
     *
     * @return a String with some embedded newlines.
     */
    def prettyPrint: String = Partnerships.prettyPrint(this)
  }


  /**
   * Companion object for Partnerships.
   */
  object Partnerships extends DefaultJsonProtocol {
    implicit val partnershipsFormat: RootJsonFormat[Partnerships] = jsonFormat1(apply)

    def prettyPrint(p: Partnerships): String = p.toJson.prettyPrint
  }

  it should "support fixed header" in {
    val strings = List("Adam,Sullivan", "Amy,Avagadro", "Ann,Peterson", "Barbara,Goldman")
    val pty: IO[Table[Player]] = Table.parse[Table[Player]](strings.iterator)
    matchIO(pty) {
      case pt@HeadedTable(_, _) =>
        val sht: Table[Partnership] = Player.convertTable(pt)
        val partnerships: Partnerships = Partnerships((for (t <- sht) yield t.asArray).toArray)
        partnerships.size shouldBe 2
        partnerships.partners.head shouldBe Array("Adam S", "Amy A")
        partnerships.partners.last shouldBe Array("Ann P", "Barbara G")
    }
  }

  it should "support fixed header and write to Json" in {
    val strings = List("Adam,Sullivan", "Amy,Avagadro", "Ann,Peterson", "Barbara,Goldman")
    val pty: IO[Table[Player]] = Table.parse[Table[Player]](strings.iterator)
    matchIO(pty) {
      case pt@HeadedTable(_, _) =>
        val sht: Table[Partnership] = Player.convertTable(pt)
        implicit val r: Renderer[Table[Partnership], String] = new JsonTableRenderer[Partnership] {}
        val z: String = implicitly[Renderer[Table[Partnership], String]].render(sht)
        z shouldBe "{\n  \"rows\": [{\n    \"playerA\": \"Adam S\",\n    \"playerB\": \"Amy A\"\n  }, {\n    \"playerA\": \"Ann P\",\n    \"playerB\": \"Barbara G\"\n  }],\n  \"header\": [\"first\", \"last\"]\n}"
    }
  }
}
