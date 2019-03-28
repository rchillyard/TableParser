package com.phasmidsoftware

package object parse {

  // CONSIDER moving this definition and renaming it
  def cellReader[T](implicit cellParser: CellParser[T]): CellParser[T] = cellParser

  type StringList = List[String]

}
