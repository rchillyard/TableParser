package com.phasmidsoftware

package object format {

  //  type Row = (Seq[String], Seq[String])

  def cellReader[T](implicit reader: CellReader[T]): CellReader[T] = reader


}
