package com.phasmidsoftware.tableparser.spark

import com.phasmidsoftware.tableparser.core.examples.Movie
import com.phasmidsoftware.tableparser.core.table.Header
import org.apache.spark.api.java.function.MapFunction
import org.apache.spark.sql.{Dataset, Encoder, Encoders, SparkSession}
import scala.util.{Failure, Success, Try}

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

object DatasetMapper extends App {
  implicit val spark: SparkSession = SparkSession.builder.appName("DatasetMapper").master("local[*]").getOrCreate()
  implicit val encoder: Encoder[Movie] = Encoders.product[Movie]
  import com.phasmidsoftware.tableparser.core.examples.Movie._

  val parser = MovieTableParser.rowParser
  val header = Header.create((Movie.header.split(',')): _*)

  new DatasetMapper[Movie](parser.parse(header))(Movie.missing).doMain("/Users/rhillyard/IdeaProjects/TableParser/spark/src/main/resources/com/phasmidsoftware/tableparser/spark/movie_metadata.csv")

}
