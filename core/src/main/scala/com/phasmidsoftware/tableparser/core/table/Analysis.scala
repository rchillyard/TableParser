package com.phasmidsoftware.tableparser.core.table

import com.phasmidsoftware.tableparser.core.examples.crime.Crime
import com.phasmidsoftware.tableparser.core.parse.{RawTableParser, TableParser, TableParserException}
import com.phasmidsoftware.tableparser.core.util.FP
import com.phasmidsoftware.tableparser.core.util.FP.sequence
import java.net.URL
import java.nio.file.Path
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
 * Case class to represent the analysis of a table.
 *
 * @param rows      the number of rows.
 * @param columns   the number of columns.
 * @param columnMap a map of column names to Column objects (the statistics of a column).
 */
case class ColumnStatistics(rows: Int, columns: Int, columnMap: Map[String, Column]) {
  /**
   * Returns a string representation of the `ColumnStatistics` object, including its row count,
   * column count, and a formatted view of the column map.
   *
   * @return a string in the format "ColumnStatistics: rows: <row count>, columns: <column count>, <column map details>".
   */
  override def toString: String = s"ColumnStatistics: rows: $rows, columns: $columns, $showColumnMap"

  private def showColumnMap: String = {
    val sb = new StringBuilder("\ncolumns:\n")
    columnMap.toSeq.foreach(t => sb.append(s"${t._1}: ${t._2}\n"))
    sb.toString()
  }
}

/**
 * Companion object for ColumnStatistics.
 * Provides a deprecated alias for backward compatibility.
 */
object ColumnStatistics {
  /**
   * Deprecated: use ColumnStatistics directly.
   * This exists only for source compatibility with code that was using Analysis(table).
   */
  @deprecated("Use ColumnStatistics or Analysis.forCsv directly", "1.4.0")
  def apply(rows: Int, columns: Int, columnMap: Map[String, Column]): ColumnStatistics =
    new ColumnStatistics(rows, columns, columnMap)
}

/**
 * Unsealed trait for polymorphic analysis of table sources.
 * Implement this trait to add support for new sources (CSV, Parquet, etc.).
 */
trait Analyzer {
  /**
   * Compute statistics for the source.
   *
   * @return a ColumnStatistics object.
   */
  def analyze(): ColumnStatistics
}

/**
 * Analyzer for CSV sources.
 * Operates on an in-memory RawTable and walks rows to compute statistics.
 *
 * @param table the raw table to analyze.
 */
case class CsvAnalyzer(table: RawTable) extends Analyzer {
  def analyze(): ColumnStatistics = {
    def createColumnMap(names: Seq[String]): Seq[(String, Column)] =
      for (name <- names; column <- Column.make(table, name)) yield name -> column

    val columnMap = for (ws <- table.maybeColumnNames.toSeq; z <- createColumnMap(ws)) yield z
    ColumnStatistics(table.size, table.head.ws.size, columnMap.toMap)
  }
}

/**
 * Provider trait for computing column statistics from external sources (e.g., Parquet files).
 * Implement this in modules that add support for new sources.
 */
trait ColumnStatisticsProvider {
  /**
   * Compute statistics for a single numeric column in a source file.
   * Returns None if the column is non-numeric or does not exist.
   * For sources with metadata, may return eager or lazy statistics.
   *
   * @param path            the path to the source file.
   * @param columnName      the name of the column to analyze.
   * @param useMetadataOnly if true, only use metadata (no row scan); if false, allow lazy fallback.
   * @return an optional Column with statistics (eager or lazy).
   */
  def computeStatistics(path: Path, columnName: String, useMetadataOnly: Boolean = true): Option[Column]
}

/**
 * The `Analysis` object provides factory methods to analyze tables from various sources.
 *
 * Example usage:
 * {{{
 *   // Analyze a CSV-sourced RawTable
 *   val stats: ColumnStatistics = Analysis(rawTable)
 *
 *   // Analyze a CSV file by Path
 *   implicit val parser: RawTableParser = RawTableParser()
 *   val stats: ColumnStatistics = Analysis.forCsv(Path.of("data.csv"))
 *
 *   // Analyze a Parquet file (requires parquet module in scope)
 *   val stats: ColumnStatistics = Analysis.forParquet[YellowTaxiTrip](Path.of("data.parquet"))
 * }}}
 */
object Analysis {

  /**
   * Analyze a CSV-sourced RawTable.
   * This is the primary method for analyzing in-memory tables.
   *
   * @param table the raw table to be analyzed.
   * @return a ColumnStatistics object containing rows, columns, and column-level statistics.
   */
  def apply(table: RawTable): ColumnStatistics =
    CsvAnalyzer(table).analyze()

  /**
   * Analyze a CSV file by Path.
   * Parses the file using the provided RawTableParser, then analyzes the result.
   *
   * @param path        the path to a CSV file.
   * @param tableParser the RawTableParser to use. Defaults to RawTableParser().
   * @return a ColumnStatistics object.
   */
  def forCsv(path: Path)(implicit tableParser: RawTableParser = RawTableParser()): ColumnStatistics =
    tableParser.parse(Try(Source.fromFile(path.toFile))) match {
      case Success(rawTable: RawTable) =>
        CsvAnalyzer(rawTable).analyze()
      case Failure(ex) =>
        throw new TableParserException(s"Failed to parse CSV file at $path", Some(ex))
    }

  /**
   * Analyze a Parquet file by Path.
   * Requires the parquet module to be in scope: `import com.phasmidsoftware.tableparser.parquet._`
   * This provides the implicit analyzer factory needed to construct a ParquetAnalyzer.
   *
   * @param path the path to a .parquet file or dataset directory.
   * @tparam Row the target case class type.
   * @param analyzerFactory implicit factory (Path => Analyzer) provided by parquet module.
   * @return a ColumnStatistics object.
   */
  def forParquet[Row <: Product](path: Path)(implicit analyzerFactory: Path => Analyzer): ColumnStatistics =
    analyzerFactory(path).analyze()
}

/**
 * A representation of the analysis of a column.
 *
 * @param clazz           a String denoting which class (maybe which variant of class) this column may be represented as.
 * @param optional        if true then this column contains nulls (empty strings).
 * @param maybeStatistics an optional MaybeStatistics (eager or lazy); None if statistics are unavailable or deferred.
 */
case class Column(clazz: String, optional: Boolean, maybeStatistics: Option[MaybeStatistics]) {
  /**
   * Converts the current `Column` instance into a descriptive string representation.
   *
   * The output string is generated based on the following rules:
   * - If the `optional` field is true, the string starts with "optional ".
   * - Appends the `clazz` value.
   * - If `maybeStatistics` contains a value, forces evaluation and appends the statistics.
   *
   * @return a string representation of the `Column` instance. This includes the class type, optionality,
   *         and any associated statistics (if present).
   */
  override def toString: String = {
    val sb = new StringBuilder
    if (optional) sb.append("optional ")
    sb.append(clazz)
    maybeStatistics match {
      case Some(ms) =>
        ms.getStatistics() match {
          case Some(s) =>
            sb.append(s" $s")
          case None =>
        }
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
   * Statistics are computed eagerly (CSV case).
   *
   * Complexity: O(N) where N is the length of xs.
   *
   * @param xs a sequence of String values, each corresponding to the column value of a row of the table.
   * @return an optional Column.
   */
  def make(xs: Seq[String]): Option[Column] = {
    val (ws, nulls) = xs.partition(_.nonEmpty)
    val nullable: Boolean = nulls.nonEmpty
    val co1 = for (xs <- sequence(for (w <- ws) yield w.toIntOption); ys = xs map (_.toDouble)) yield
      Column("Int", nullable, Statistics.make(ys).map(EagerStatistics(_)))
    lazy val co2 = for (xs <- sequence(for (w <- ws) yield w.toDoubleOption); ys = xs) yield
      Column("Double", nullable, Statistics.make(ys).map(EagerStatistics(_)))
    co1 orElse co2 orElse Some(Column("String", nullable, None))
  }

  /**
   * Compute statistics for a single numeric column from an external source via a provider.
   * The provider (typically from the parquet module) is passed implicitly.
   * Optionally allows lazy evaluation instead of eager metadata-only lookup.
   *
   * @param path            the path to the source file.
   * @param columnName      the name of the column to analyze.
   * @param useMetadataOnly if true, only use metadata (return None if unavailable);
   *                        if false, allow lazy fallback for expensive computation.
   * @param provider        the ColumnStatisticsProvider, usually implicit.
   * @return an optional Column with statistics computed (eager or lazy).
   */
  def statisticsFrom(path: Path, columnName: String, useMetadataOnly: Boolean = true)(implicit provider: ColumnStatisticsProvider): Option[Column] =
    provider.computeStatistics(path, columnName, useMetadataOnly)
}

/**
 * Sealed trait representing statistics that may be eager or lazy.
 * Eager statistics are computed upfront (from Parquet metadata).
 * Lazy statistics are deferred as a thunk (computed on demand).
 */
sealed trait MaybeStatistics {
  /**
   * Force evaluation of statistics.
   * If eager, returns immediately. If lazy, executes the thunk.
   *
   * @return an optional Statistics (None if computation fails or thunk returns None).
   */
  def getStatistics(): Option[Statistics]
}

/**
 * Eager statistics, computed upfront (typically from Parquet metadata).
 *
 * @param stats the computed Statistics.
 */
case class EagerStatistics(stats: Statistics) extends MaybeStatistics {
  def getStatistics(): Option[Statistics] = Some(stats)
}

/**
 * Lazy statistics, deferred as a thunk for later evaluation.
 * Useful for expensive computations (e.g., column scan) that you may not need.
 *
 * @param compute a function that computes the Statistics when called.
 */
case class LazyStatistics(compute: () => Option[Statistics]) extends MaybeStatistics {
  def getStatistics(): Option[Statistics] = compute()
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
      case Success(t) =>
        System.err.println(s"Unexpected result: $t")
      case Failure(x) =>
        throw x
    }
  }
}