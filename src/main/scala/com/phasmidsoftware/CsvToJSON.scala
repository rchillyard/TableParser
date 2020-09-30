/*
 * Copyright (c) 2020. Phasmid Software
 */

package com.phasmidsoftware

import java.io.File
import java.io.PrintWriter

import com.phasmidsoftware.parse.{CellParser, CellParsers, StringTableParserWithHeader, TableParser}
import com.phasmidsoftware.render.Writable
import com.phasmidsoftware.table.Table
import spray.json.{DefaultJsonProtocol, JsValue, RootJsonFormat, enrichAny}

import scala.util.Try

object CsvToJSON extends App {

  case class Pair(north: String, south: String) {
    def asArray: Array[String] = Array(north, south)
  }

  case class SharkBridgePairings(partners: Array[Array[String]])

  object Pair extends CellParsers {
    implicit val pairParser: CellParser[Pair] = cellParser2(Pair.apply)
  }

  /**
    * TODO: understand why this requires the headers (north, south) to be in all caps in the input file.
   */
  implicit val ptp: TableParser[Table[Pair]] = StringTableParserWithHeader[Pair]()

  val userHome = System.getProperty("user.home")
  val pty: Try[Table[Pair]] = Table.parse[Table[Pair]](new File(s"$userHome/Documents/pairs.csv"))
  val sy: Try[SharkBridgePairings] = for (pt <- pty) yield SharkBridgePairings((for (r <- pt.rows) yield r.asArray).toArray)

  object PairingsJsonProtocol extends DefaultJsonProtocol {
    implicit val sharkBridgePairings: RootJsonFormat[SharkBridgePairings] = jsonFormat1(SharkBridgePairings)
  }

  import PairingsJsonProtocol._

  val jy: Try[JsValue] = for (s <- sy) yield s.toJson

  val pw: PrintWriter = new PrintWriter(s"$userHome/Documents/pairings.txt")
  for (j <- jy) pw.println(j)
  pw.close()

  jy recover {
    case e => println(e.getLocalizedMessage)
  }
}
