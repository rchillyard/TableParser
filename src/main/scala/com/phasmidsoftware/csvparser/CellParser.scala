package com.phasmidsoftware.csvparser

import scala.util.Try

trait CellParser[T] {

  def parse(w: String): Try[T]

}
