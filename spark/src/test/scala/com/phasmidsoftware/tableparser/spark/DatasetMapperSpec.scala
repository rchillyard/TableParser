package com.phasmidsoftware.tableparser.spark

import com.phasmidsoftware.tableparser.core.examples.Movie
import com.phasmidsoftware.tableparser.core.table.Header
import org.apache.spark.sql.{Dataset, Encoder, Encoders, SparkSession}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DatasetMapperSpec extends AnyFlatSpec with should.Matchers with Serializable {

  behavior of "DatasetMapper"

  it should "test" in {
    implicit val spark: SparkSession = SparkSession.builder.appName("DatasetMapper").master("local[*]").getOrCreate()
    implicit val encoder: Encoder[Movie] = Encoders.product[Movie]
    import com.phasmidsoftware.tableparser.core.examples.Movie._

    val parser = MovieTableParser.rowParser
    val header = Header.create((Movie.header.split(',')): _*)

    val target = new DatasetMapper[Movie](parser.parse(header))(Movie.missing)
    // NOTE: I don't know if there's a way to specify a classpath resource in Spark so, for now, we define a totally non-portable filename
    val filename = "/Users/rhillyard/IdeaProjects/TableParser/spark/src/main/resources/com/phasmidsoftware/tableparser/spark/movie_metadata.csv"
    val dataset: Dataset[Movie] = target.as(filename)
    dataset.show(20)
    dataset.count() should be(20000)
  }
}
