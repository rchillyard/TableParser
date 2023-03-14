/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.table

import scala.annotation.unused

/**
 * Trait which transforms an X into a Y.
 * Basically, this is just a sub-class of Function1.
 *
 * @tparam X the input type.
 * @tparam Y the output type.
 */
trait Transformation[X, Y] extends (X => Y)

case class RawTableTransformation(transformers: Map[String, Transformation[String, String]]) extends Transformation[RawTable, RawTable] {
  def apply(t: RawTable): RawTable = {
    val xm: Map[Int, Transformation[String, String]] = for ((k, x) <- transformers; h <- t.maybeHeader; index <- h.getIndex(k).toOption) yield (index, x)
    t.map(RawRowTransformation(xm))
  }
}

/**
 * TESTME
 *
 * @param aggregators a Map of Transformations indexed by String.
 */
@unused
case class RawTableAggregation(aggregators: Map[String, Transformation[String, String]]) extends Transformation[RawTable, RawTable] {
  def apply(t: RawTable): RawTable = {
    val header = t.maybeHeader.get // there must be a header for a raw table.
    // TODO avoid use of "get" (after we have created a unit test for this)
    val xm: Map[Int, Transformation[String, String]] = for ((k, x) <- aggregators; index = header.getIndex(k).get) yield (index, x)
    t.map(RawRowTransformation(xm))
  }
}

case class RawTableProjection(columns: Seq[String]) extends Transformation[RawTable, RawTable] {
  def apply(t: RawTable): RawTable = {
    val xs: Seq[Int] = for (k <- columns; h <- t.maybeHeader; io = h.getIndex(k).toOption; i <- io) yield i
    t.map(RawRowProjection(xs))
  }
}

case class RawRowTransformation(transformers: Map[Int, Transformation[String, String]]) extends Transformation[RawRow, RawRow] {

  def apply(r: RawRow): RawRow = RawRow(for ((x, i) <- r.ws.zipWithIndex; f = transformers.getOrElse(i, identity[String] _)) yield f(x), r.header)
}

case class RawRowProjection(columns: Seq[Int]) extends Transformation[RawRow, RawRow] {
  def apply(r: RawRow): RawRow = RawRow(for ((x, i) <- r.ws.zipWithIndex; if columns contains i) yield x, r.header)
}

case class CellTransformation[X, Y](f: X => Y) extends Transformation[X, Y] {
  def apply(x: X): Y = f(x)
}