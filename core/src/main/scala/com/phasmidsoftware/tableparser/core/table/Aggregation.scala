/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.table

// CONSIDER nothing has referenced this since it was written in 2019, and the two case classes
// that would have used it are commented out below. Decide whether it is a direction still
// intended or should be deleted (#225).
trait Aggregation[X] extends (Seq[X] => X)

//case class RowAggregation(transformers: Map[String, CellTransformation[_]]) extends Aggregation[Row]

//case class CellAggregation[X](column: String) extends Aggregation[X]