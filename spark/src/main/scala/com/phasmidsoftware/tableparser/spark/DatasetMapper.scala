package com.phasmidsoftware.tableparser.spark

import com.phasmidsoftware.tableparser.core.examples.{Format, Movie, Production}
import com.phasmidsoftware.tableparser.core.parse.StandardRowParser
import com.phasmidsoftware.tableparser.core.table.Header
import org.apache.spark.api.java.function.MapFunction
import org.apache.spark.sql.catalyst.encoders.encoderFor
import org.apache.spark.sql.{Dataset, Encoder, Encoders, SparkSession}
import scala.util.{Failure, Success}

object DatasetMapper extends App {

  val spark: SparkSession = SparkSession.builder.appName("DatasetMapper").master("local[*]").getOrCreate()

  val sd: Dataset[String] = spark.read.textFile("/Users/rhillyard/IdeaProjects/TableParser/spark/src/main/resources/com/phasmidsoftware/tableparser/spark/movie_metadata.csv")

  import com.phasmidsoftware.tableparser.core.examples.MovieParser._

  val parser = MovieTableParser.rowParser

  val header = Header.create((Movie.header.split(',')):_*)

  private val mapFunction = new MapFunction[String, Movie] {
    def call(value: String): Movie =
      parser.parse(header)(value) match {
        case Success(movie) => movie
        case Failure(exception) =>
          System.err.println(exception.getLocalizedMessage)
          Movie.missing
      }
  }

  private val encoder: Encoder[Movie] = Encoders.product[Movie]

  val result = sd.map(mapFunction, encoder)

  result.show(20)
}
