package com.phasmidsoftware.tableparser.spark

import com.phasmidsoftware.tableparser.spark.DatasetParser.doMain
import org.apache.spark.sql.SparkSession
import org.scalatest.flatspec.AnyFlatSpec

class DatasetParserSpec extends AnyFlatSpec {

  behavior of "DatasetParser"

  // TODO figure out why this doesn't work: we do the same thing in DatasetParser and it works fine.
  ignore should "doMain" in {
    val spark: SparkSession = SparkSession.builder.appName("DatasetParser").master("local[*]").getOrCreate()
    doMain(spark)
  }

}
