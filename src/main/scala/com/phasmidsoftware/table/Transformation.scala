/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.table

import com.phasmidsoftware.RawRow

trait Transformation[X, Y] extends (X => Y) {

//  def map[Z](f: Y=>Z): Transformation[X, Z]
}

case class RawTableTransformation(transformers: Map[String, Transformation[String, String]]) extends Transformation[RawTable, RawTable] {
  override def apply(t: RawTable): RawTable = {
    val header = t.maybeHeader.get // there must be a header for a raw table.
    // TODO fix this get
    val xm: Map[Int, Transformation[String, String]] = for ((k, x) <- transformers; index = header.getIndex(k).get) yield (index, x)
    t.map[RawRow](RawRowTransformation(xm))
  }

//  def map[Z](f: RawTable => Z): Transformation[RawTable, Z] = f compose apply
}

/**
  * TEST this
  *
  * @param aggregators a Map of Transformations indexed by String.
  */
case class RawTableAggregation(aggregators: Map[String, Transformation[String, String]]) extends Transformation[RawTable, RawTable] {
  override def apply(t: RawTable): RawTable = {
    val header = t.maybeHeader.get // there must be a header for a raw table.
    // TODO fix this get
    val xm: Map[Int, Transformation[String, String]] = for ((k, x) <- aggregators; index = header.getIndex(k).get) yield (index, x)
    t.map[RawRow](RawRowTransformation(xm))
  }
}

case class RawTableProjection(columns: Seq[String]) extends Transformation[RawTable, RawTable] {
  override def apply(t: RawTable): RawTable = {
    val header = t.maybeHeader.get // there must be a header for a raw table.
    // TODO fix this get
    val xs: Seq[Int] = for (k <- columns; index = header.getIndex(k).get) yield index
    t.map[RawRow](RawRowProjection(xs))
  }
}

case class RawRowTransformation(transformers: Map[Int, Transformation[String, String]]) extends Transformation[RawRow, RawRow] {
  override def apply(xs: RawRow): RawRow = for ((x, i) <- xs.zipWithIndex; f = transformers.getOrElse(i, identity[String] _)) yield f(x)
}

case class RawRowProjection(columns: Seq[Int]) extends Transformation[RawRow, RawRow] {
  override def apply(xs: RawRow): RawRow = for ((x, i) <- xs.zipWithIndex; if columns contains i) yield x
}

case class CellTransformation[X, Y](f: X => Y) extends Transformation[X, Y] {
  override def apply(x: X): Y = f(x)
}