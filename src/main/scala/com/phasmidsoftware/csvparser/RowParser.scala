package com.phasmidsoftware.csvparser

import scala.util.Try

trait RowParser {

  def parse(w: String)(header: Seq[String]): Try[Row]

  def parseHeader(w: String): Seq[String]
}
