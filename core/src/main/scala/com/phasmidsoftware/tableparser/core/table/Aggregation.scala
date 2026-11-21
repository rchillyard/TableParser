/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.core.table

// TODO delete this. Nothing has referenced it since it was written in 2019, and the two case
// classes that would have used it are commented out below. It is public, so removing it breaks
// binary compatibility and cannot go on a patch release; it is deferred to 2.6.0, and the MiMa
// filter it will need is recorded beside previousRelease in build.sbt (#225).
trait Aggregation[X] extends (Seq[X] => X)

//case class RowAggregation(transformers: Map[String, CellTransformation[_]]) extends Aggregation[Row]

//case class CellAggregation[X](column: String) extends Aggregation[X]