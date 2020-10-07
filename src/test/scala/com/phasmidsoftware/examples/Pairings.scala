/*
 * Copyright (c) 2020. Phasmid Software
 */

package com.phasmidsoftware.examples

import com.phasmidsoftware.parse.{CellParser, TableParserHelper}
import com.phasmidsoftware.table.Table

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

  private val (inputFile, outputFile) = getFileNames("Documents", "partnerships")
  private val pty = Table.parse[Table[Player]](scala.io.Source.fromFile(inputFile))
  private val tsy = for (pss <- for (pt <- pty) yield pt.rows grouped 2) yield for (ps <- pss) yield Partnership(ps)
  private val sy = for (ts <- tsy) yield Partnerships((for (t <- ts) yield t.asArray).toArray)
  sy.foreach(w => println(s"${w.size} partnerships read from $inputFile"))
  (sy map (_.prettyPrint)).transform(outputPairingString, outputException)

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
  lazy val nickname: String = s"$first ${last.head}"
}

/**
  * Companion object for Player.
  */
object Player extends TableParserHelper[Player]() {
  lazy val cellParser: CellParser[Player] = cellParser2(apply)
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
  */
object Partnership {
  /**
    * Factory method to create a Partnership from a sequence of two players.
    *
    * @param players the sequence of Players. Whatever length is given, we will use the nicknames of the first and last.
    * @return a Partnership.
    */
  def apply(players: Seq[Player]): Partnership = Partnership(players.head.nickname, players.last.nickname)
}

/**
  * A case class representing all Partnerships as an Array of Array of String.
  * This is the structure required by the Shark Bridge app.
  *
  * @param partnerships an Array of two-element Arrays of Strings.
  */
case class Partnerships(partnerships: Array[Array[String]]) {
  lazy val size: Int = partnerships.length

  /**
    * Method to output these Partnerships as a Json String.
    *
    * @return a String with some embedded newlines.
    */
  lazy val prettyPrint: String = Partnerships.prettyPrint(this)
}

import spray.json.{DefaultJsonProtocol, RootJsonFormat, enrichAny}

/**
  * Companion object for Partnerships.
  */
object Partnerships extends DefaultJsonProtocol {
  implicit val partnershipsFormat: RootJsonFormat[Partnerships] = jsonFormat1(apply)

  def prettyPrint(p: Partnerships): String = p.toJson.prettyPrint
}
