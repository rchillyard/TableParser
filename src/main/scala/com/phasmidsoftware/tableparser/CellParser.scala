package com.phasmidsoftware.tableparser

import com.phasmidsoftware.format.FormatsException

trait CellParser[T] {
  // Need to define this better so that we don't have any non-implemented methods.
  def convertString(w: String): T

  def read(value: Convertible): T = value match {
    case CellValue(w) => convertString(w)
    case RowValues(row, columns) => read(row, columns)
    case _ => throw FormatsException(s"CellParser: cannot convert value $value of type ${value.getClass}")
  }

  def read(row: Row, columns: Seq[String]): T
}

trait SingleCellParser[T] extends CellParser[T] {
  def read(row: Row, columns: Seq[String]): T = ???
}

trait MultiCellParser[T] extends CellParser[T] {
  def convertString(w: String): T = ???
}

sealed abstract class Convertible {
  def convertTo[T: CellParser]: T = cellReader.read(this)
}

case class CellValue(w: String) extends Convertible

case class RowValues(row: Row, ws: Seq[String]) extends Convertible

object RowValues {
  def apply(row: Row): RowValues = RowValues(row, row.hdr)
}
