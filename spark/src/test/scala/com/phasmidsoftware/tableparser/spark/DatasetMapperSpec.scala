package com.phasmidsoftware.tableparser.spark

import com.phasmidsoftware.tableparser.core.examples.Movie
import org.apache.spark.sql.{Dataset, Encoder, Encoders, SparkSession}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DatasetMapperSpec extends AnyFlatSpec with should.Matchers with Serializable {

  behavior of "DatasetMapper"

  it should "create a Movie Dataset" in {
    implicit val spark: SparkSession = SparkSession.builder.appName("DatasetMapper").master("local[*]").getOrCreate()
    implicit val encoder: Encoder[Movie] = Encoders.product[Movie]

    val target = new DatasetMapper[Movie](MovieDatabase.parser.parse(MovieDatabase.header))(Movie.missing)
    val dataset: Dataset[Movie] = target.as(MovieDatabase.filename)
    dataset.show(20)
    dataset.count() should be(1610)
  }
}
