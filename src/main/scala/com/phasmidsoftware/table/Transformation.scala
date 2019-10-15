/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.table

trait Transformation[X, Y] extends (X => Y) {

}

case class TableTransformation[Row1, Row2](f: Row1 => Row2)

//case class RowTransformation(transformers: Map[String, CellTransformation[_,_]]) extends Transformation[Row, Row] {
//  override def apply(v1: Row): Row = v1.
//}

case class CellTransformation[X, Y](column: String, f: X => Y) extends Transformation[X, Y] {
  override def apply(x: X): Y = f(x)
}