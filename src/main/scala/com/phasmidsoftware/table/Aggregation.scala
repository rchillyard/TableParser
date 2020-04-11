/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.table

trait Aggregation[X] extends (Seq[X] => X)

//case class RowAggregation(transformers: Map[String, CellTransformation[_]]) extends Aggregation[Row]

//case class CellAggregation[X](column: String) extends Aggregation[X]