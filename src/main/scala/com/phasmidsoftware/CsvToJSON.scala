/*
 * Copyright (c) 2020. Phasmid Software
 */

package com.phasmidsoftware

import java.io.{File, PrintWriter}

import com.phasmidsoftware.parse.{CellParser, CellParsers, StringTableParserWithHeader, TableParser}
import com.phasmidsoftware.table.Table
import spray.json.{DefaultJsonProtocol, RootJsonFormat, enrichAny}

import scala.util.{Success, Try}

object CsvToJSON extends App {

  case class Pair(north: String, south: String) {
    def asArray: Array[String] = Array(north, south)
  }

  case class SharkBridgePairings(partners: Array[Array[String]]) {
    def size: Int = partners.length
  }

  object Pair extends CellParsers {
    implicit val pairParser: CellParser[Pair] = cellParser2(Pair.apply)
  }

  /**
    * TODO: understand why this requires the headers (north, south) to be in all caps in the input file.
    */
  implicit val ptp: TableParser[Table[Pair]] = StringTableParserWithHeader[Pair]()

  private val userHome = System.getProperty("user.home")
  private val inputFile = s"$userHome/Documents/pairs.csv"
  private val outputFile = s"$userHome/Documents/pairings.txt"
  val pty: Try[Table[Pair]] = Table.parse[Table[Pair]](new File(inputFile))
  val sy: Try[SharkBridgePairings] = for (pt <- pty) yield SharkBridgePairings((for (r <- pt.rows) yield r.asArray).toArray)

  for (s <- sy) println(s"${s.size} partnerships read from $inputFile")

  object PairingsJsonProtocol extends DefaultJsonProtocol {
    implicit val sharkBridgePairings: RootJsonFormat[SharkBridgePairings] = jsonFormat1(SharkBridgePairings)
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
