package com.phasmidsoftware

package object parse {

  def cellReader[T](implicit reader: CellParser[T]): CellParser[T] = reader

  type StringList = List[String]

}
