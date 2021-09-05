package com.phasmidsoftware.render

import com.phasmidsoftware.parse.{CellParser, TableParserHelper}
import com.phasmidsoftware.table.{HeadedTable, Header, Table, TableJsonFormat}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import spray.json._

import scala.util.{Success, Try}

class JsonTableRendererSpec extends AnyFlatSpec with should.Matchers {

  implicit object PartnershipTableRenderer$ extends JsonTableRenderer[Partnership]

  behavior of "JsonTableRenderer"

  it should "render Partnership table" in {
    val strings = List("First, Last", "Adam,Sullivan", "Amy,Avagadro", "Ann,Peterson", "Barbara,Goldman")
    val wy: Try[String] = for (pt <- Table.parse[Table[Player]](strings); q = Player.convertTable(pt); w = q.asInstanceOf[Renderable[Partnership]].render) yield w
    wy should matchPattern { case Success("{\n  \"rows\": [{\n    \"playerA\": \"Adam S\",\n    \"playerB\": \"Amy A\"\n  }, {\n    \"playerA\": \"Ann P\",\n    \"playerB\": \"Barbara G\"\n  }],\n  \"header\": [\"playerA\", \"playerB\"]\n}") => }
    implicit val r: JsonFormat[Table[Partnership]] = new TableJsonFormat[Partnership] {}
    wy.map(p => p.parseJson.convertTo[Table[Partnership]]) should matchPattern { case Success(HeadedTable(_, _)) => }
  }

  case class Player(first: String, last: String) {
    def nickname: String = s"$first ${last.head}"
  }

  object Player extends TableParserHelper[Player]() with DefaultJsonProtocol {
    def cellParser: CellParser[Player] = cellParser2(apply)

    implicit val playerFormat: RootJsonFormat[Player] = jsonFormat2(apply)

    /**
      * Method to transform a Table[Player] into a Table[Partnership].
      *
      * The requirements of the application are that the rows of the Player table are grouped by twos
      * and each resulting entity (an array of length 2) is taken to form a Partnership.
      *
      * @param pt a Table[Player]
      * @return a Table[Partnership]
      */
    def convertTable(pt: Table[Player]): Table[Partnership] = pt.processRows(xs => (xs grouped 2).toList).map(r => Converters.convert(r)).replaceHeader(Some(Header.create("playerA", "playerB")))
  }

  case class Partnership(playerA: String, playerB: String)

  object Partnership extends DefaultJsonProtocol {
    implicit val partnershipFormat: RootJsonFormat[Partnership] = jsonFormat2(apply)
  }

  object Converters {
    def convert(players: Iterable[Player]): Partnership = Partnership(players.head.nickname, players.last.nickname)
  }

}

