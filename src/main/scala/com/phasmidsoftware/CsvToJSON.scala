/*
 * Copyright (c) 2020. Phasmid Software
 */

package com.phasmidsoftware

import java.io.File

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

  implicit val ptp: TableParser[Table[Pair]] = StringTableParserWithHeader[Pair]()

  implicit object StringBuilderWriteable extends Writable[StringBuilder] {
    override def unit: StringBuilder = new StringBuilder

    override def delimiter: CharSequence = "|"

    override def writeRaw(o: StringBuilder)(x: CharSequence): StringBuilder = o.append(x.toString)
  }

  val pty: Try[Table[Pair]] = Table.parse[Table[Pair]](new File("/Users/rhillyardx/Documents/pairs.csv"))

  val sy: Try[SharkBridgePairings] = for (pt <- pty) yield SharkBridgePairings((for (r <- pt.rows) yield r.asArray).toArray)

  object PairingsJsonProtocol extends DefaultJsonProtocol {
    implicit val sharkBridgePairings: RootJsonFormat[SharkBridgePairings] = jsonFormat1(SharkBridgePairings)
  }

  import PairingsJsonProtocol._

  val jy: Try[JsValue] = for (s <- sy) yield s.toJson

  for (j <- jy) println(j)

  jy recover {
    case e => println(e.getLocalizedMessage)
  }
}
