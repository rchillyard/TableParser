/*
 * Copyright (c) 2020. Phasmid Software
 */

package com.phasmidsoftware

import java.io.{File, PrintWriter}

import com.phasmidsoftware.parse.{CellParser, CellParsers, ColumnHelper, StringTableParserWithHeader, TableParser}
import com.phasmidsoftware.table.{Header, Table}
import spray.json.{DefaultJsonProtocol, RootJsonFormat, enrichAny}

import scala.io.Codec
import scala.util.{Success, Try}

object CsvToJSON extends App {

  case class Player(first: String, last: String) {
    def nickname: String = s"$first ${last.head}"
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

  object Player extends CellParsers {
    implicit val playerParser: CellParser[Player] = cellParser2(Player.apply)
  }

  implicit val ptp: TableParser[Table[Player]] = StringTableParserWithHeader[Player](Some(Header[Player]()))
  private val userHome = System.getProperty("user.home")
  private val baseName = if (args.length > 0) args.head else "partnerships"
  private val inputFile = s"$userHome/Documents/$baseName.csv"
  private val outputFile = s"$userHome/Documents/$baseName.json"
  implicit val codec: Codec = Codec.UTF8
  val pty: Try[Table[Player]] = Table.parse[Table[Player]](new File(inputFile))
  val pssy: Try[Iterator[Seq[Player]]] = for (pt <- pty) yield pt.rows grouped 2
  val tsy: Try[Iterator[Partnership]] = for (pss <- pssy) yield for (ps <- pss) yield Partnership(ps)
  val sy: Try[Partnerships] = for (ts <- tsy) yield Partnerships((for (t <- ts) yield t.asArray).toArray)
  for (s <- sy) println(s"${s.size} partnerships read from $inputFile")

  object PairingsJsonProtocol extends DefaultJsonProtocol {
    implicit val partnershipsFormat: RootJsonFormat[Partnerships] = jsonFormat1(Partnerships)
  }

  import PairingsJsonProtocol._

  def printJson(w: String): Try[Unit] = {
    val p: PrintWriter = new PrintWriter(outputFile)
    p.println(w)
    p.close()
    println(s"output sent to $outputFile")
    Success(())
  }

  (for (s <- sy; j = s.toJson) yield j.prettyPrint).transform(printJson, e => Success(println(e.getLocalizedMessage)))
}
