package com.phasmidsoftware.table

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.table.Statistics.{makeHistogram, makeNumeric}
import com.phasmidsoftware.util.FP
import com.phasmidsoftware.util.FP.sequence
import scala.collection.mutable
import scala.io.Source

/**
 * Class to represent the analysis of a table.
 *
 * @param rows      the number of rows.
 * @param columns   the number of columns.
 * @param columnMap a map of column names to Column objects (the analytics of a column).
 */
case class Analysis(rows: Int, columns: Int, columnMap: Map[String, Column]) {
  override def toString: String = s"Analysis: rows: $rows, columns: $columns, $showColumnMap"

  def showColumnMap: String = {
    val sb = new StringBuilder("\ncolumns:\n")
    columnMap.toSeq.foreach(t => sb.append(s"${t._1}: ${t._2}\n"))
    sb.toString()
  }
}

object Analysis {

  def apply(table: RawTable): Analysis = {
    /**
     * Method to create a column map, i.e. a sequence of String->Column pairs.
     *
     * Complexity of this statement is W * X where W is the number of columns and X is the time to make a Column object.
     *
     * @param names a sequence of column names.
     * @return a sequence of String,Column tuples.
     */
    def createColumnMap(names: Seq[String]): Seq[(String, Column)] = for (name <- names; column <- Column.make(table, name)) yield name -> column

    val columnMap = for (ws <- table.maybeColumnNames.toSeq; z <- createColumnMap(ws)) yield z
    new Analysis(table.size, table.head.ws.size, columnMap.toMap)
  }
}

/**
 * A representation of the analysis of a column.
 *
 * @param clazz         a String denoting which class (maybe which variant of class) this column may be represented as.
 * @param optional      if true then this column contains nulls (empty strings).
 * @param maybeAnalytic an optional Analytic but only if the column represents something which can be analyzed.
 */
case class Column(clazz: String, optional: Boolean, maybeAnalytic: Option[Analytic]) {
  override def toString: String = {
    val sb = new StringBuilder
    if (optional) sb.append("optional ")
    sb.append(clazz)
    maybeAnalytic match {
      case Some(s) => sb.append(s" $s")
      case _ =>
    }
    sb.toString()
  }
}

object Column {
  /**
   * Method to make a Column from column values of <code>table</code>, identified by <code>name</code>.
   * Some columns cannot be analyzed (e.g., non-numeric columns) and that's why the result is optional.
   *
   * The complexity of this method is O(N) where N is the number of rows in the table.
   *
   * @param table the (raw) table from which the column is to be analyzed.
   * @param name  the name of the column.
   * @return an optional Column.
   */
  def make(table: RawTable, name: String): Option[Column] = sequence(table.column(name)) flatMap (ws => make(ws))

  /**
   * Method to make a Column, the analysis of a column of a (raw) Table.
   * If the column is numeric (can be parsed as integers or doubles), then we can create a result, otherwise not.
   *
   * Complexity: O(N) where N is the length of xs.
   *
   * @param xs an sequence of String values, each corresponding to the column value of a row of the table.
   * @return an optional Column.
   */
  def make(xs: Seq[String]): Option[Column] = {
    val (ws, nulls) = xs.partition(_.nonEmpty)
    val nullable: Boolean = nulls.nonEmpty
    // CONSIDER we can combine the following two lines
    val co1 = for (xs <- sequence(for (w <- ws) yield w.toIntOption); ys = xs map (_.toDouble)) yield Column("Int", nullable, makeNumeric(ys))
    lazy val co2 = for (xs <- sequence(for (w <- ws) yield w.toDoubleOption); ys = xs) yield Column("Double", nullable, makeNumeric(ys))
    lazy val maybeHistogram: Option[Analytic] = makeHistogram(ws)
    co1 orElse co2 orElse Some(Column("String", nullable, maybeHistogram))
  }
}

trait Analytic

/**
 * Class to represent the statistics of a numerical column.
 *
 * @param mu    the mean value.
 * @param sigma the standard deviation.
 * @param min   the smallest value.
 * @param max   the largest value.
 */
case class Statistics(mu: Double, sigma: Double, min: Double, max: Double) extends Analytic {
  override def toString: String = s"(range: $min-$max, mean: $mu, stdDev: $sigma)"
}

case class Histogram[K](keyFreq: Map[K, Int]) extends Analytic {
  override def toString: String = keyFreq.toSeq.sortBy(x => x._2).reverse.map { case (k, n) => s"$k: $n" }.mkString("\n")
}

object Statistics {
  /**
   * Make an (optional) Statistics object for a sequence of Double.
   * CONSIDER defining the underlying type as a parametric type with context bound Numeric.
   *
   * @param xs a sequence of Double.
   * @return an optional Statistics.
   */
  def makeNumeric(xs: Seq[Double]): Option[Statistics] = xs match {
    case Nil => None
    case h :: Nil => Some(Statistics(h, 0, h, h))
    case _ => doMakeNumeric(xs)
  }

  /**
   * Make an (optional) Histogram object for a sequence of String.
   * CONSIDER defining the underlying type as a parametric type.
   *
   * @param xs a sequence of String.
   * @return an optional Histogram.
   */
  def makeHistogram(xs: Seq[String], ratio: Int = 10): Option[Histogram[String]] = {
    val m: mutable.Map[String, Int] = mutable.HashMap[String, Int]()
    xs foreach {
      x =>
        val freq = m.getOrElse(x, 0)
        m.put(x, freq + 1)
    }
    if (m.size < xs.size / ratio) Some(Histogram(m.toMap))
    else None
  }

  private def doMakeNumeric(xs: Seq[Double]): Option[Statistics] = {
    val mu = xs.sum / xs.size
    val variance = (xs map (_ - mu) map (x => x * x)).sum / xs.size
    Some(Statistics(mu, math.sqrt(variance), xs.min, xs.max))
  }
}

object Main extends App {
  // TODO merge the two copies of this file into one (it needs to be at the root level of resources)
  val crimeFile = "2023-01-metropolitan-street-sample.csv"

  // Set up the source
  val sy: IO[Source] = IO.fromTry(for (u <- FP.resource[Analysis](crimeFile)) yield Source.fromURL(u))

  val fraction = 1
  // Set up the parser (we set the predicate only for demonstration purposes)
  val parser: RawTableParser = RawTableParser().setPredicate(TableParser.sampler(fraction))

  parser.parse(sy).unsafeRunSync() match {
    case t@HeadedTable(r, _) =>
      val analysis = Analysis(t)
      println(s"Crime: $analysis")
      r take 10 foreach println
  }
}
