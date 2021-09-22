package com.phasmidsoftware.table

import com.phasmidsoftware.util.FP
import com.phasmidsoftware.util.FP.sequence

case class Analysis(rows: Int, columns: Int, columnMap: Map[String, Column]) {
  override def toString: String = s"Analysis: rows: $rows, columns: $columns, column map: $showColumnMap"

  def showColumnMap: String = {
    val sb = new StringBuilder()
    columnMap.toSeq.foreach(t => sb.append(s"${t._1}: ${t._2}\n"))
    sb.toString()
  }
}

object Analysis {
  def apply(table: Table[Seq[String]]): Analysis = {
    val wso: Option[Seq[String]] = table.maybeColumnNames

    def method1(ws: Seq[String]): Seq[(String, Column)] = for (w <- ws; z <- FP.sequence(table.column(w)).toSeq; q <- Column.make(z).toSeq) yield w -> q

    val columnMap: Iterable[(String, Column)] = for (ws <- wso.toSeq; z <- method1(ws)) yield z
    new Analysis(table.size, table.head.size, columnMap.toMap)
  }
}

case class Column(clazz: String, maybeStatistics: Option[Statistics])

object Column {
  def make(xs: Iterator[String]): Option[Column] = {
    val ws = xs.toList
    val co1: Option[Column] = for (xs <- sequence(for (w <- ws) yield w.toIntOption); ys = xs map (_.toDouble)) yield Column("Int", Statistics.make(ys))
    lazy val co2: Option[Column] = for (xs <- sequence(for (w <- ws) yield w.toDoubleOption); ys = xs) yield Column("Double", Statistics.make(ys))
    co1 orElse co2 orElse Some(Column("String", None))
  }
}

case class Statistics(mu: Double, sigma: Double, min: Double, max: Double) {
  override def toString: String = s"(range: $min-$max, mean: $mu, stdDev: $sigma)"
}

object Statistics {
  def doMake(xs: Seq[Double]): Option[Statistics] = {
    val mu = xs.sum / xs.size
    val sigma = 0 // FIXME
    Some(Statistics(mu, sigma, xs.min, xs.max))
  }

  def make(xs: Seq[Double]): Option[Statistics] = xs match {
    case Nil => None
    case h :: Nil => Some(Statistics(h, 0, h, h))
    case _ => doMake(xs)
  }
}

