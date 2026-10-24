package com.phasmidsoftware.tableparser.core.table

import com.phasmidsoftware.tableparser.core.examples.crime.Crime
import com.phasmidsoftware.tableparser.core.parse.{RawTableParser, TableParser}
import com.phasmidsoftware.tableparser.core.util.FP
import java.net.URL
import scala.io.Source
import scala.util.{Failure, Success, Try}

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
object Main:
  // NOTE: not `extends App`: in Scala 3, the body of an App runs when the object is initialized
  // (e.g. by a call of doMain), and its parallel foreach then deadlocks on that initialization.
  def main(args: Array[String]): Unit =
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
  def doMain(triedUrl: Try[URL]): Unit =
    // Set up the source
    val sy: Try[Source] = for (u <- triedUrl) yield Source.fromURL(u)

    val fraction = 1
    // Set up the parser (we set the predicate only for demonstration purposes)
    val parser: RawTableParser = RawTableParser().setPredicate(TableParser.sampler(fraction))

    parser.parse(sy) match
      case Success(t@HeadedTable(r, _)) =>
        val analysis = Analysis(t)
        println(s"Crime: $analysis")
        r take 10 foreach println
      case Success(t) =>
        System.err.println(s"Unexpected result: $t")
      case Failure(x) =>
        throw x
