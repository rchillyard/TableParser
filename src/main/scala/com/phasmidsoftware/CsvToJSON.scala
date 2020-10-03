/*
 * Copyright (c) 2020. Phasmid Software
 */

package com.phasmidsoftware

import java.io.PrintWriter

import com.phasmidsoftware.parse.{CellParser, CellParsers, StringTableParserWithHeader, TableParser}
import com.phasmidsoftware.table.Table
import spray.json.{DefaultJsonProtocol, RootJsonFormat, enrichAny}

import scala.util.{Success, Try}

/**
  * Main Application which is really just an example application and should be defined in the test classes.
  */
object CsvToJSON extends App {

  case class Player(first: String, last: String) {
    def nickname: String = s"$first ${last.head}"
  }

  object Player extends CellParsers {
    implicit val playerParser: CellParser[Player] = cellParser2(Player.apply)
  }

  def parsePlayerTable(inputFile: String): Try[Table[Player]] = {
    // NOTE: use the following form if the source file does NOT contain an explicit header row AND if the file has
    // columns which are in the same order as the attributes of Player.
    //  implicit val ptp: TableParser[Table[Player]] = StringTableParserWithHeader.create[Player]

    // NOTE: use the following form if the source file does contain an explicit header row.
    implicit val ptp: TableParser[Table[Player]] = StringTableParserWithHeader[Player]()

    Table.parseFile[Table[Player]](inputFile)
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

  val (inputFile, outputFile) = getFileNames("Documents", "partnerships")
  val pty: Try[Table[Player]] = parsePlayerTable(inputFile)
  val pssy: Try[Iterator[Seq[Player]]] = for (pt <- pty) yield pt.rows grouped 2
  val tsy: Try[Iterator[Partnership]] = for (pss <- pssy) yield for (ps <- pss) yield Partnership(ps)
  val sy: Try[Partnerships] = for (ts <- tsy) yield Partnerships((for (t <- ts) yield t.asArray).toArray)
  for (s <- sy) println(s"${s.size} partnerships read from $inputFile")

  object PairingsJsonProtocol extends DefaultJsonProtocol {
    implicit val partnershipsFormat: RootJsonFormat[Partnerships] = jsonFormat1(Partnerships)
  }

  import PairingsJsonProtocol._

  private val triedString: Try[String] = for (s <- sy; j = s.toJson) yield j.prettyPrint
  triedString.transform(printJson, e => Success(System.err.println(e.getLocalizedMessage)))

  private def printJson(w: String): Try[Unit] = {
    val p: PrintWriter = new PrintWriter(outputFile)
    p.println(w)
    p.close()
    println(s"output of ${w.length} characters sent to $outputFile")
    Success(())
  }

  private def getFileNames(baseDirectory: String, defaultBaseName: String): (String, String) = {
    val userHome = System.getProperty("user.home")
    val baseName = if (args.length > 0) args.head else defaultBaseName
    val inputFile = s"$userHome/$baseDirectory/$baseName.csv"
    val outputFile = s"$userHome/$baseDirectory/$baseName.json"
    (inputFile, outputFile)
  }
}
