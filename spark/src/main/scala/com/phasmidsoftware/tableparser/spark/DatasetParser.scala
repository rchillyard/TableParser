package com.phasmidsoftware.tableparser.spark

import com.phasmidsoftware.tableparser.core.examples.Movie
import com.phasmidsoftware.tableparser.core.parse.TableParser
import com.phasmidsoftware.tableparser.core.table.Table
import com.phasmidsoftware.tableparser.core.util.FP.resource
import java.net.URL
import org.apache.spark.sql.{Dataset, Encoder, SparkSession}
import scala.io.Codec
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/**
 * The `DatasetParser` class is a utility designed to parse structured data and create a Spark `Dataset` of a specific type `T`.
 * It relies on an implicit Spark session and an implicit `TableParser` for transforming raw data into a `Table` representation
 * before converting it into a Spark `Dataset`. This class provides a safe and reusable way to load and parse datasets from resources such as URLs.
 *
 * @param sparkSession an implicit `SparkSession` used for managing Spark operations.
 * @param tableParser  an implicit `TableParser` instance that defines how to parse the raw data into a `Table[T]`.
 * @tparam T the type of the elements in the resulting `Dataset`. It should have an implicit Encoder available for serialization.
 *
 *           Usage example:
 * {{{
 * implicit val spark: SparkSession = SparkSession.builder.appName("DatasetParser").master("local[*]").getOrCreate()
 * import spark.implicits._
 *
 * // TableParser instance for the specific data type
 * implicit val movieTableParser: StringTableParser[Table[Movie]] = implicitly[MovieTableParser]
 *
 * val parser = new DatasetParser[Movie]()
 * parser.createDataset[DatasetParser[_]]("movie_metadata.csv") match {
 *   case Success(ds) => ds.show(10)
 *   case Failure(error) => throw error
 * }
 * }}}
 */
class DatasetParser[T: Encoder]()(implicit sparkSession: SparkSession, tableParser: TableParser[Table[T]]) {

  /**
   * Creates a Spark `Dataset` of type `T` by parsing a given resource specified by its name.
   * This method utilizes an implicit `TableParser` to parse the raw data into a structured table
   * and subsequently converts the parsed table into a Spark `Dataset`. It handles potential errors using `Try`.
   *
   * @param name the name of the resource to be loaded, such as a file name in the classpath.
   * @tparam U the class type associated with locating the resource. A `ClassTag` for `U` is required.
   * @return a `Try[Dataset[T]]`, indicating success with the parsed dataset or failure with an appropriate error.
   */
  def createDataset[U: ClassTag](name: String): Try[Dataset[T]] = for {
    url <- resource[U](name)
    ds <- parse(url)
  } yield ds

  /**
   * Parses a given URL to create a Spark `Dataset` of type `T`.
   * Utilizes an implicit `Codec` to handle potential encoding issues during parsing and
   * an implicit `TableParser` to interpret the raw data into a structured table.
   * The table is then converted into a Spark `Dataset` and wrapped in a `Try` to handle any potential errors.
   *
   * @param url   the `URL` pointing to the resource to parse.
   * @param codec an implicit `Codec` used to decode the content of the resource.
   * @return a `Try[Dataset[T]]`, containing the parsed Spark `Dataset` on success or an exception on failure.
   */
  def parse(url: URL)(implicit codec: Codec): Try[Dataset[T]] =
    Table.parseResource(url) map {
      tt: Table[T] =>
        println(s"Table has ${tt.size} rows")
        sparkSession.createDataset(tt.toSeq)
  }
}

/**
 * The `DatasetParser` object demonstrates how to parse and process a CSV file containing movie metadata
 * into a structured dataset using Spark and an implicit `StringTableParser`.
 * It showcases capabilities to handle parsing, error management, and displaying results.
 *
 * This object extends the `App` trait, making it directly executable as a Scala program.
 */
object DatasetParser extends App {

  /**
   * Method to demonstrate parsing a CSV file containing movie metadata into a dataset.
   * This method uses an implicit `StringTableParser` to parse the data and creates a dataset of `Movie` entities.
   * If parsing is successful, it displays the first 10 rows of the dataset; otherwise, it throws an exception.
   *
   * @param sparkSession an implicit SparkSession required to execute Spark operations.
   * @return Unit; this method performs side effects like displaying the dataset and does not return any value.
   */
  def doMain(implicit sparkSession: SparkSession): Unit = {
    import com.phasmidsoftware.tableparser.core.examples.Movie._
    import spark.implicits._

    new DatasetParser[Movie]().createDataset[DatasetParser[_]]("movie_metadata.csv") match {
      case Success(ds) =>
        ds.show(10)
      case Failure(x) =>
        throw x
    }
  }

  val spark: SparkSession = SparkSession.builder().appName("DatasetParser").master("local[*]").getOrCreate()

  doMain(spark)
}
