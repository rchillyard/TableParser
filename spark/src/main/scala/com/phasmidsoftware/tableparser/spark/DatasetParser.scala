package com.phasmidsoftware.tableparser.spark

import com.phasmidsoftware.tableparser.core.examples.Movie
import com.phasmidsoftware.tableparser.core.parse.{StringTableParser, TableParser}
import com.phasmidsoftware.tableparser.core.table.Table
import com.phasmidsoftware.tableparser.core.util.FP.resource
import java.net.URL
import org.apache.spark.sql.{Dataset, Encoder, SparkSession}
import scala.io.Codec
import scala.util.{Failure, Success, Try}

class DatasetParser[T: Encoder]()(implicit sparkSession: SparkSession, tableParser: TableParser[Table[T]]) {

  def parse(url: URL)(implicit codec: Codec): Try[Dataset[T]] = {
    val triedTable: Try[Table[T]] = Table.parseResource(url)
    triedTable map {
      mt: Table[T] =>
        println(s"Table has ${mt.size} rows")
        sparkSession.createDataset(mt.toSeq)
    }
  }
}

object DatasetParser extends App {
  implicit val spark: SparkSession = SparkSession.builder.appName("DatasetParser").master("local[*]").getOrCreate()

  import com.phasmidsoftware.tableparser.core.examples.MovieParser._

  implicit val movieTableParser: StringTableParser[Table[Movie]] = implicitly[MovieTableParser]
  import spark.implicits._

  (for {
    url <- resource[DatasetParser[_]]("movie_metadata.csv")
    ds <- new DatasetParser[Movie]().parse(url)
  } yield ds) match {
    case Success(ds) =>
      ds.show(10)
    case Failure(x) =>
      throw x
  }
}
