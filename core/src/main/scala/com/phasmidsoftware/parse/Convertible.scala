package com.phasmidsoftware.parse

import com.phasmidsoftware.table.{Header, Row}
import scala.util.Try

/**
 * Trait Convertible.
 */
sealed trait Convertible {
  def convertTo[T: CellParser]: Try[T] = cellReader.parse(this)
}

/**
 * A concrete Convertible corresponding to a cell value.
 *
 * @param w the String contained by a cell.
 */
case class CellValue(w: String) extends Convertible

/**
 * A concrete Convertible corresponding to a row.
 *
 * @param row    the Row containing several values.
 * @param header the Header.
 */
case class RowValues(row: Row, header: Header) extends Convertible {
  override def toString: String = (for ((h, x) <- header.xs zip row.ws) yield s"""${h.toUpperCase}="$x"""").mkString(", ")
}

object RowValues {
  def apply(row: Row): RowValues = RowValues(row, row.hdr)
}
