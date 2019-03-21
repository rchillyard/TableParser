package com.phasmidsoftware.tableparser

import scala.util.Try

trait RowParser[Row] {

  def parse(w: String)(header: Seq[String]): Try[Row]

  def parseHeader(w: String): Seq[String]
}

case class Row(ws: Seq[String], hdr: Seq[String]) extends (String => String) {
  /**
    * Method to yield the value for a given column name
    *
    * @param w the column name
    * @return the value as a String
    * @throws IndexOutOfBoundsException if w is not contained in hdr or hdr is longer than ws.
    */
  override def apply(w: String): String = ws(hdr.indexOf(w))
}
