package com.phasmidsoftware.table

import com.phasmidsoftware.util.FP
import com.phasmidsoftware.util.FP.sequence

import scala.util.Try

case class Analysis(rows: Int, columns: Int, columnMap: Map[String, Column]) {
  override def toString: String = s"Analysis: rows: $rows, columns: $columns, $showColumnMap"

  def showColumnMap: String = {
    val sb = new StringBuilder("\ncolumns:\n")
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

/**
  * A representation of the analysis of a column.
  *
  * @param clazz           a String denoting which class (maybe which variant of class) this column may be represented as.
  * @param optional        if true then this column contains nulls (empty strings).
  * @param maybeStatistics an optional set of statistics but only if the column represents numbers.
  */
case class Column(clazz: String, optional: Boolean, maybeStatistics: Option[Statistics]) {
  override def toString: String = {
    val sb = new StringBuilder
    if (optional) sb.append("optional ")
    sb.append(clazz)
    maybeStatistics match {
      case Some(s) => sb.append(s" $s")
      case _ =>
    }
    sb.toString()
  }
}

object Column {
  def make(xs: Iterator[String]): Option[Column] = {
    val (ws, nulls) = xs.toList.partition(_.nonEmpty)
    val optional = nulls.nonEmpty
    // 2.12 the following two lines
    val co1: Option[Column] = for (xs <- sequence(for (w <- ws) yield Try(w.toInt).toOption); ys = xs map (_.toDouble)) yield Column("Int", optional, Statistics.make(ys))
    lazy val co2: Option[Column] = for (xs <- sequence(for (w <- ws) yield Try(w.toDouble).toOption); ys = xs) yield Column("Double", optional, Statistics.make(ys))
    co1 orElse co2 orElse Some(Column("String", optional, None))
  }
}

case class Statistics(mu: Double, sigma: Double, min: Double, max: Double) {
  override def toString: String = s"(range: $min-$max, mean: $mu, stdDev: $sigma)"
}

object Statistics {
  def doMake(xs: Seq[Double]): Option[Statistics] = {
    val mu = xs.sum / xs.size
    val variance = (xs map (_ - mu) map (x => x * x)).sum / xs.size
    Some(Statistics(mu, math.sqrt(variance), xs.min, xs.max))
  }

  def make(xs: Seq[Double]): Option[Statistics] = xs match {
    case Nil => None
    case h :: Nil => Some(Statistics(h, 0, h, h))
    case _ => doMake(xs)
  }
}

