package com.phasmidsoftware.tableparser

import scala.util.Try

trait RowParser[Row] {

  def parse(w: String)(header: Seq[String]): Try[Row]

  def parseHeader(w: String): Seq[String]
}
