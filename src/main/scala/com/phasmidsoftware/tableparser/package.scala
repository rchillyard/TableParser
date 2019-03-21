package com.phasmidsoftware

package object tableparser {

  def cellReader[T](implicit reader: CellParser[T]): CellParser[T] = reader

}
