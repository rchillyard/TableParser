/*
 * Copyright (c) 2020. Phasmid Software
 */

package com.phasmidsoftware

import java.io.PrintWriter

import com.phasmidsoftware.parse.{CellParser, TableParserHelper}
import com.phasmidsoftware.table.Table
import spray.json.{DefaultJsonProtocol, RootJsonFormat, enrichAny}

import scala.util.{Success, Try}

/**
  * NOTE: see Pairings for an improved version of this.
  *
  * Main Application which is really just an example application and should be defined in the test classes.
  */
object CsvToJSON extends App {

  case class Player(first: String, last: String) {
    def nickname: String = s"$first ${last.head}"
  }

  object Player extends TableParserHelper[Player]() {
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

  println(s"CsvToJSON ${args.headOption.getOrElse("")}")

  val (inputFile, outputFile) = getFileNames("Documents", "Partnerships")

  val pty: Try[Table[Player]] = Table.parseFile[Table[Player]](inputFile)
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
