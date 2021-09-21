package com.phasmidsoftware.table

import scala.reflect.ClassTag

case class Analysis(rows: Int, columns: Int, columnMap: Map[String, Column]) {
  override def toString: String = s"Analysis: rows: $rows, columns: $columns, column map: $columnMap"
}

case class Column(classTag: ClassTag[Any], maybeStatistics: Option[Statistics])

case class Statistics(mu: Double, sigma: Double, min: Double, max: Double)

object Analysis {
  def apply(table: Table[Seq[String]]): Analysis = new Analysis(table.size, table.head.size, Map())
}
