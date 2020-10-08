/*
 * Copyright (c) 2020. Phasmid Software
 */

package com.phasmidsoftware.examples

import com.phasmidsoftware.parse.{CellParser, TableParserHelper}
import com.phasmidsoftware.render.JsonRenderer
import com.phasmidsoftware.table.Table
import spray.json.{DefaultJsonProtocol, JsArray, JsonWriter, enrichAny}

import scala.util.{Success, Try}

/**
  * Pairings is an example application which reads a CSV file and outputs JSON.
  * The file is assumed to be in ~/Documents/Partnerships.csv and the output will be in a file with the same
  * base name but extension .json
  *
  * The pplication which reads a CSV file of player names, then forms partnerships by pairing
  * the odd-numbered lines with the even-numbered lines.
  * As written, it is expected that
  * the input file has a header row with at least "first" and "last" columns.
  * However, it is easy to switch to a programmed header--see the definition of TableParserHelper.
  *
  * Each pairing is then converted to a Partnership (as required by the Shark Bridge Teacher Console).
  *
  * The resulting partnerhips are then output as Json, by first converting the Iterator[Partnship] to a Array[Partnership].
  *
  * You can override the name "Partnerships" by specifying an appropriate command-line argument.
  *
  * NOTE: see Pairings project for an improved version of this.
  */
object Pairings extends App {

  println(s"Pairings ${args.headOption.getOrElse("")}")

  import Partnership._
  import Player._

  private val (inputFile, outputFile) = getFileNames("Documents", "partnerships")
  private val pty = Table.parse[Table[Player]](scala.io.Source.fromFile(inputFile))
  private val tsy = for (pt <- pty) yield Player.convertTable(pt)
  tsy.foreach(w => println(s"${w.size} partnerships read from $inputFile"))
  private val wy = for (ts <- tsy) yield ts.render
  wy.transform(outputPairingString, outputException)

  private def outputPairingString(w: String): Try[Unit] = {
    import java.io.PrintWriter
    val p: PrintWriter = new PrintWriter(outputFile)
    p.println(w)
    p.close()
    println(s"output of ${w.length} sent to $outputFile")
    Success(())
  }

  private def outputException(e: Throwable) = Success(System.err.println(e.getLocalizedMessage))

  private def getFileNames(baseDirectory: String, defaultBaseName: String): (String, String) = {
    val userHome = System.getProperty("user.home")
    val baseName = if (args.length > 0) args.head else defaultBaseName
    val inputFile = s"$userHome/$baseDirectory/$baseName.csv"
    val outputFile = s"$userHome/$baseDirectory/$baseName.json"
    (inputFile, outputFile)
  }
}

/**
  * This case class represents a Player with first name, last name.
  *
  * @param first the first name.
  * @param last  the last name.
  */
case class Player(first: String, last: String) {
  def nickname: String = s"$first ${last.head}"
}

/**
  * Companion object for Player.
  */
object Player extends TableParserHelper[Player]() {
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

/**
  * Case class to represent a Partnership represented by two Strings.
  *
  * @param playerA the first player.
  * @param playerB the second player.
  */
case class Partnership(playerA: String, playerB: String) {
  lazy val asArray: Array[String] = Array(playerA, playerB)
}

/**
  * Companion object to Partnership.
  * NOTE: this is more complex than usual because the application requires that the names of the two players of the Partnership
  * be converted into an array of String.
  */
object Partnership extends DefaultJsonProtocol {

  implicit val partnershipWriter: JsonWriter[Partnership] = (p: Partnership) => JsArray(p.asArray.map(_.toJson).toVector)

  implicit val partnershipRenderer: JsonRenderer[Partnership] = new JsonRenderer[Partnership] {}

  /**
    * Factory method to create a Partnership from a sequence of two players.
    *
    * @param players the sequence of Players. Whatever length is given, we will use the nicknames of the first and last.
    * @return a Partnership.
    */
  def apply(players: Iterable[Player]): Partnership = Partnership(players.head.nickname, players.last.nickname)
}
