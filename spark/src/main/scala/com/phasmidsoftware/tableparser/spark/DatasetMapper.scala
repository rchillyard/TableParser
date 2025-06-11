package com.phasmidsoftware.tableparser.spark

import com.phasmidsoftware.tableparser.core.examples.Movie
import com.phasmidsoftware.tableparser.core.parse.{HeadedCSVTableParser, RowParser}
import com.phasmidsoftware.tableparser.core.table.Header
import com.phasmidsoftware.tableparser.spark.MovieDatabase.{filename, header}
import org.apache.spark.api.java.function.MapFunction
import org.apache.spark.sql.{Dataset, Encoder, Encoders, SparkSession}
import scala.util.{Failure, Success, Try}

/**
 * A generic class for mapping and transforming datasets from text-based input to structured data of type `T`
 * using Apache Spark.
 *
 * This class enables parsing and conversion of raw textual data into Spark `Dataset`s, applying a
 * user-defined mapping function with error handling for missing or malformed data.
 *
 * @param f            a function that attempts to parse a `String` into an instance of type `T`.
 *                     It returns a `Try[T]`, allowing for robust error handling during parsing.
 * @param missing      a fallback value of type `T` to use if parsing fails for a given input row.
 * @param sparkSession an implicit `SparkSession`, required for Spark operations.
 * @param encoder      an implicit `Encoder[T]`, which enables serialization of the resulting `Dataset[T]`.
 * @tparam T the type of the elements in the resulting Spark `Dataset`.
 */
class DatasetMapper[T](f: String => Try[T])(missing: T)(implicit sparkSession: SparkSession, encoder: Encoder[T]) extends Serializable {

  def as(path: String): Dataset[T] = {

    val mapFunction = new MapFunction[String, T] {
      def call(value: String): T =
        f(value) match {
          case Success(t) => t
          case Failure(exception) =>
            System.err.println(exception.getLocalizedMessage)
            missing
        }
    }

    val sd = sparkSession.read.textFile(path)
    sd.map(mapFunction, encoder)
  }

  def doMain(filename: String): Unit = {
    val sd: Dataset[T] = as(filename)
    sd.show(20)
  }
}

/**
 * The `DatasetMapper` object is the entry point for mapping and processing datasets using Spark.
 * It leverages the `DatasetMapper` generic class to parse and transform text-based data into structured datasets.
 * This object is specifically tailored to process data of type `Movie` using the provided parser and configuration.
 *
 * It sets up the necessary implicit Spark session and encoders, and executes the main logic for dataset transformation.
 *
 * Functionality Overview:
 * - Initializes a Spark session configured for local execution.
 * - Configures the `DatasetMapper` to use the `MovieDatabase` parser for processing rows of movie data.
 * - Defines default handling for missing or malformed data using the `Movie.missing` value.
 * - Invokes the dataset processing logic with a specified input file containing raw text data, displaying the first 20 rows of the resulting dataset.
 *
 * Note: This object is designed to run as a standalone Spark application to demonstrate dataset mapping functionality.
 *
 */
object DatasetMapper extends App {
  implicit val spark: SparkSession = SparkSession.builder.appName("DatasetMapper").master("local[*]").getOrCreate()
  implicit val encoder: Encoder[Movie] = Encoders.product[Movie]
  new DatasetMapper[Movie](MovieDatabase.parser.parse(header))(Movie.missing).doMain(filename)
}

/**
 * The MovieDatabase object provides utility functions and constants for handling movie metadata.
 * It serves as a central repository for defining parsers and file paths required for processing movie-related data.
 *
 * This object includes mechanisms for parsing movie data and manages access to a CSV file containing movie metadata.
 */
object MovieDatabase {

  import com.phasmidsoftware.tableparser.core.examples.Movie._

  val parser: RowParser[Movie, String] = implicitly[HeadedCSVTableParser[Movie]].rowParser
  val header: Header = Header.create((Movie.header.split(',')): _*)

  // NOTE: I don't know if there's a way to specify a classpath resource in Spark so, for now, we define a totally non-portable filename
  private val home = System.getProperties.getOrDefault("user.home", "/Users/rhillyardXX")
  private val projects = "/IdeaProjects"
  private val tableParserDirectory = "/TableParser"
  /**
   * Represents the relative path to the CSV file containing movie metadata.
   * This path is used as a resource location within the application's project structure
   * where the file can be accessed for parsing and dataset creation.
   * It should not change.
   */
  private val resource = "/spark/src/main/resources/com/phasmidsoftware/tableparser/spark/movie_metadata.csv"
  val filename: String = home + projects + tableParserDirectory + resource
}