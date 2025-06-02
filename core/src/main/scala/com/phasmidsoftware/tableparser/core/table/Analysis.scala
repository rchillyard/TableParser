package com.phasmidsoftware.tableparser.core.table

import com.phasmidsoftware.tableparser.core.examples.crime.Crime
import com.phasmidsoftware.tableparser.core.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.tableparser.core.util.FP
import com.phasmidsoftware.tableparser.core.util.FP.sequence
import java.net.URL
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
 * Class to represent the analysis of a table.
 *
 * @param rows      the number of rows.
 * @param columns   the number of columns.
 * @param columnMap a map of column names to Column objects (the statistics of a column).
 */
case class Analysis(rows: Int, columns: Int, columnMap: Map[String, Column]) {
  /**
   * Returns a string representation of the `Analysis` object, including its row count,
   * column count, and a formatted view of the column map.
   *
   * @return a string in the format "Analysis: rows: <row count>, columns: <column count>, <column map details>".
   */
  override def toString: String = s"Analysis: rows: $rows, columns: $columns, $showColumnMap"

  private def showColumnMap: String = {
    val sb = new StringBuilder("\ncolumns:\n")
    columnMap.toSeq.foreach(t => sb.append(s"${t._1}: ${t._2}\n"))
    sb.toString()
  }
}

/**
 * The `Analysis` object provides functionality to analyze a given raw table and produce an `Analysis`
 * instance, representing statistical insights such as the number of rows, number of columns, and detailed
 * metadata for each column (if analysable).
 */
object Analysis {

  /**
   * Applies analysis on the given raw table to generate an Analysis object. The analysis includes deriving
   * metrics such as the number of rows, columns, and a column map that represents the relationship between
   * column names and their respective analyzed `Column` objects.
   *
   * @param table the raw table to be analyzed, providing data and metadata required for column insights.
   * @return an Analysis object containing rows, columns, and a map of column names to corresponding `Column` objects.
   */
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
 * @param clazz           a String denoting which class (maybe which variant of class) this column may be represented as.
 * @param optional        if true then this column contains nulls (empty strings).
 * @param maybeStatistics an optional set of statistics but only if the column represents numbers.
 */
case class Column(clazz: String, optional: Boolean, maybeStatistics: Option[Statistics]) {
  /**
   * Converts the current `Column` instance into a descriptive string representation.
   *
   * The output string is generated based on the following rules:
   * - If the `optional` field is true, the string starts with "optional ".
   * - Appends the `clazz` value.
   * - If `maybeStatistics` contains a value, appends the string representation of the statistics.
   *
   * @return a string representation of the `Column` instance. This includes the class type, optionality,
   *         and any associated statistics (if present).
   */
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
   * @param xs a sequence of String values, each corresponding to the column value of a row of the table.
   * @return an optional Column.
   */
  def make(xs: Seq[String]): Option[Column] = {
    val (ws, nulls) = xs.partition(_.nonEmpty)
    val nullable: Boolean = nulls.nonEmpty
    val co1 = for (xs <- sequence(for (w <- ws) yield w.toIntOption); ys = xs map (_.toDouble)) yield Column("Int", nullable, Statistics.make(ys))
    lazy val co2 = for (xs <- sequence(for (w <- ws) yield w.toDoubleOption); ys = xs) yield Column("Double", nullable, Statistics.make(ys))
    co1 orElse co2 orElse Some(Column("String", nullable, None))
  }
}

/**
 * Class to represent the statistics of a column.
 *
 * @param mu    the mean value.
 * @param sigma the standard deviation.
 * @param min   the smallest value.
 * @param max   the largest value.
 */
case class Statistics(mu: Double, sigma: Double, min: Double, max: Double) {
  /**
   * Returns a string representation of the `Statistics` object, including the range, mean,
   * and standard deviation of the data.
   *
   * @return a string in the format "(range: min-max, mean: mu, stdDev: sigma)"
   *         where `min`, `max`, `mu`, and `sigma` are the respective values of the `Statistics` object.
   */
  override def toString: String = s"(range: $min-$max, mean: $mu, stdDev: $sigma)"
}

/**
 * Companion object for the Statistics case class.
 * Provides methods to create a `Statistics` instance from a sequence of doubles.
 */
object Statistics {
  /**
   * Creates an `Option[Statistics]` from a sequence of `Double` values.
   * If the sequence is empty, returns `None`.
   * If the sequence contains one value, returns a `Statistics` instance with zero standard deviation and the same value for min and max.
   * If the sequence contains more than one value, delegates to a private method to calculate the statistics.
   *
   * @param xs a sequence of `Double` values from which to compute statistics.
   * @return an `Option[Statistics]` containing the computed statistics if the sequence is non-empty, or `None` if the sequence is empty.
   */
  def make(xs: Seq[Double]): Option[Statistics] = xs match {
    case Nil =>
      None
    case h :: Nil =>
      Some(Statistics(h, 0, h, h))
    case _ =>
      doMake(xs)
  }

  private def doMake(xs: Seq[Double]): Option[Statistics] = {
    val mu = xs.sum / xs.size
    val variance = (xs map (_ - mu) map (x => x * x)).sum / xs.size
    Some(Statistics(mu, math.sqrt(variance), xs.min, xs.max))
  }
}

/**
 * The Main object serves as the entry point of the application and provides functionality
 * to parse and analyze a specified dataset of crime information.
 *
 * This object:
 * - Loads a given CSV file containing crime data.
 * - Sets up a source for reading the data.
 * - Configures a `RawTableParser` instance to parse the table data with specified predicates.
 * - Analyzes the parsed data and prints relevant results to the console.
 * - Demonstrates how to handle the parsing process using Scala's Try-Success-Failure mechanism.
 *
 * The application expects the file to be properly located in the resources directory.
 *
 * Members:
 * - `crimeFile`: Specifies the file name of the dataset (located in the resources directory).
 * - `sy`: Sets up a data source using the provided file.
 * - `fraction`: Defines a fraction value for sampling the data when parsing.
 * - `parser`: Configures and prepares the table parser with a predicate for data sampling.
 *
 * Main functionalities:
 * - Parses the source using the configured `RawTableParser`.
 * - On success: Performs analysis on the parsed data and logs the first 10 rows and summary information.
 * - On failure: Throws an exception indicating the failure reason.
 *
 * Example Usage:
 * To analyze a specific dataset, update the `crimeFile` variable with the correct
 * resource file path before running the application.
 */
object Main extends App {
  doMain(FP.resource[Crime]("2023-01-metropolitan-street-sample.csv"))

  /**
   * Executes the main logic of the program, which includes reading a crime data file,
   * setting up a table parser with a specific sampling fraction, and performing
   * data analysis on the parsed table. The results of the analysis and a subset
   * of the data rows are printed to the console.
   *
   * @return Unit - This method performs side effects (e.g., file IO, data analysis, and printing)
   *         and does not return a value.
   */
  def doMain(triedUrl: Try[URL]): Unit = {
    // Set up the source
    val sy: Try[Source] = for (u <- triedUrl) yield Source.fromURL(u)

    val fraction = 1
    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser: RawTableParser = RawTableParser().setPredicate(TableParser.sampler(fraction))

    parser.parse(sy) match {
      case Success(t@HeadedTable(r, _)) =>
        val analysis = Analysis(t)
        println(s"Crime: $analysis")
        r take 10 foreach println
      case Failure(x) =>
        throw x
    }
  }
}
