package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper.camelToSnakeCaseColumnNameMapper
import com.phasmidsoftware.tableparser.core.parse.{CellParsers, ColumnHelper}
import com.phasmidsoftware.tableparser.core.table.Table
import java.nio.file.{Path, Paths}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Success, Try}

class ParquetParserSpec extends AnyFlatSpec with Matchers with CellParsers with BeforeAndAfterAll {

  // Path to the committed test fixture
  private val samplePath: Path =
    Paths.get(getClass.getResource("/taxi_sample.parquet").toURI)

  private var table: Table[YellowTaxiTrip] = _

  // The first row in the Parquet file has vendorId=2, puLocationId=75, doLocationId=237.
  // We find it by key rather than relying on ParIterable ordering.
  private def firstRow: YellowTaxiTrip =
    table.content.toSeq
            .find(r => r.puLocationId.contains(75) && r.doLocationId.contains(237))
            .getOrElse(fail("Expected row with puLocationId=75, doLocationId=237 not found"))

  override def beforeAll(): Unit = {
    import YellowTaxiTrip.helper
    table = ParquetParser.parse[YellowTaxiTrip](samplePath).get
  }

  behavior of "YellowTaxiTrip model"

  it should "print the Parquet schema for inspection" in {
    import org.apache.hadoop.conf.Configuration
    import org.apache.hadoop.fs.{Path => HadoopPath}
    import org.apache.parquet.hadoop.ParquetFileReader
    import org.apache.parquet.hadoop.util.HadoopInputFile
    val conf = new Configuration()
    val reader = ParquetFileReader.open(
      HadoopInputFile.fromPath(new HadoopPath(samplePath.toUri), conf)
    )
    val schema = reader.getFooter.getFileMetaData.getSchema
    reader.close()
    (0 until schema.getFieldCount).foreach { i =>
      val t = schema.getType(i)
      println(s"${t.getRepetition} ${t.getName}")
    }
    succeed
  }

  behavior of "ParquetParser"

  // ── Happy path ─────────────────────────────────────────────────────────────

  it should "parse taxi_sample.parquet into a Table[YellowTaxiTrip]" in {
    import YellowTaxiTrip.helper
    val result: Try[Table[YellowTaxiTrip]] =
      ParquetParser.parse[YellowTaxiTrip](samplePath)
    result shouldBe a[Success[_]]
  }

  it should "produce exactly 1000 rows" in {
    table.size shouldBe 1000
  }

  it should "have a header with 19 columns" in {
    table.maybeHeader.map(_.size) shouldBe Some(19)
  }

  it should "parse storeAndFwdFlag correctly for the known first row" in {
    firstRow.storeAndFwdFlag shouldBe Some("N")
  }

  it should "parse tripDistance as non-negative for the known first row" in {
    firstRow.tripDistance.get should be >= 0.0
  }

  it should "parse fareAmount correctly for the known first row" in {
    firstRow.fareAmount shouldBe Some(10.0)
  }

  it should "parse totalAmount correctly for the known first row" in {
    firstRow.totalAmount shouldBe Some(19.5)
  }

  it should "have pickup before dropoff for the known first row" in {
    firstRow.tpepPickupDatetime.zip(firstRow.tpepDropoffDatetime)
            .headOption.forall { case (p, d) => p.isBefore(d) } shouldBe true
  }

  it should "have all rows with storeAndFwdFlag = Y or N" in {
    table.content.toSeq.forall(r =>
      r.storeAndFwdFlag.forall(f => f == "Y" || f == "N")
    ) shouldBe true
  }

  it should "have all rows with non-negative tripDistance" in {
    table.content.toSeq.forall(r =>
      r.tripDistance.forall(_ >= 0.0)
    ) shouldBe true
  }

  // ── Schema validation ──────────────────────────────────────────────────────

  it should "fail with ParquetParserException for an unknown column name" in {
    case class BadSchema(nonExistentColumn: String)
    implicit val badHelper: ColumnHelper[BadSchema] =
      columnHelper(camelToSnakeCaseColumnNameMapper)
    val result = ParquetParser.parse[BadSchema](samplePath)
    result.failed.get shouldBe a[ParquetParserException]
    result.failed.get.getMessage should include("non_Existent_Column")
  }

  it should "fail with ParquetParserException for a non-Option field mapping to an OPTIONAL column" in {
    // congestion_surcharge is OPTIONAL in the schema but declared as Double here
    // In the actual taxi data it happens to be non-null in all sample rows,
    // but the validator should catch the schema-level mismatch.
    // NOTE: this test assumes congestion_surcharge is OPTIONAL in the Parquet schema.
    // If it is REQUIRED in the 2024 file, adjust accordingly after inspecting the schema.
    pending
  }

  // ── Analysis ──────────────────────────────────────────────────────────────

  behavior of "Analysis"

  it should "analyze a Parquet file and extract schema without materializing rows" in {
    import com.phasmidsoftware.tableparser.core.table.Analysis
    val stats = Analysis.forParquet[YellowTaxiTrip](samplePath)
    stats.rows shouldBe 1000
    stats.columns shouldBe 19
  }

  it should "infer column types from Parquet schema" in {
    import com.phasmidsoftware.tableparser.core.table.Analysis
    val stats = Analysis.forParquet[YellowTaxiTrip](samplePath)
    // Spot-check a few columns based on TLC schema
    stats.columnMap.get("trip_distance").map(_.clazz) shouldBe Some("Double")
    stats.columnMap.get("fare_amount").map(_.clazz) shouldBe Some("Double")
    stats.columnMap.get("passenger_count").map(_.clazz) shouldBe Some("Int")
    stats.columnMap.get("store_and_fwd_flag").map(_.clazz) shouldBe Some("String")
  }

  it should "infer optionality from Parquet schema (OPTIONAL vs REQUIRED)" in {
    import com.phasmidsoftware.tableparser.core.table.Analysis
    val stats = Analysis.forParquet[YellowTaxiTrip](samplePath)
    // TLC schema has all columns as OPTIONAL
    stats.columnMap.values.forall(_.optional) shouldBe true
  }

  it should "leave statistics deferred (lazy) in Parquet schema analysis" in {
    import com.phasmidsoftware.tableparser.core.table.Analysis
    val stats = Analysis.forParquet[YellowTaxiTrip](samplePath)
    // All columns should have maybeStatistics = None (deferred, not computed during schema read)
    stats.columnMap.values.forall(_.maybeStatistics.isEmpty) shouldBe true
  }

  it should "compute statistics for a single numeric column on demand" in {
    import com.phasmidsoftware.tableparser.core.table.{Column, EagerStatistics, LazyStatistics}
    val tripDistStats = Column.statisticsFrom(samplePath, "trip_distance", useMetadataOnly = false)
    tripDistStats shouldBe a[Some[_]]
    val col = tripDistStats.get
    col.clazz shouldBe "Double"
    col.optional shouldBe true
    col.maybeStatistics shouldBe a[Some[_]]
    col.maybeStatistics match {
      case Some(LazyStatistics(statsThunk)) =>
        val stats = statsThunk().get
        stats.min should be >= 0.0
        stats.max should be >= stats.min
      case Some(EagerStatistics(stats)) =>
        stats.min should be >= 0.0
        stats.max should be >= stats.min
      case None => fail("Expected statistics for numeric column")
    }
  }

  it should "return None for non-numeric columns when computing statistics" in {
    import com.phasmidsoftware.tableparser.core.table.Column
    Column.statisticsFrom(samplePath, "store_and_fwd_flag") shouldBe None
  }

  it should "return None for non-existent columns when computing statistics" in {
    import com.phasmidsoftware.tableparser.core.table.Column
    Column.statisticsFrom(samplePath, "nonexistent_column") shouldBe None
  }

  it should "support metadata-only mode (return None if no metadata)" in {
    import com.phasmidsoftware.tableparser.core.table.Column
    Column.statisticsFrom(samplePath, "trip_distance", useMetadataOnly = true) shouldBe None
  }

  it should "support lazy fallback mode (return LazyStatistics if no metadata)" in {
    import com.phasmidsoftware.tableparser.core.table.{Column, LazyStatistics}
    val lazyStats = Column.statisticsFrom(samplePath, "trip_distance", useMetadataOnly = false)
    lazyStats shouldBe a[Some[_]]
    lazyStats.get.maybeStatistics match {
      case Some(LazyStatistics(_)) => succeed
      case _ => fail("Expected LazyStatistics when useMetadataOnly=false")
    }
  }

  // ── Dataset (multi-file) ───────────────────────────────────────────────────

  behavior of "Dataset"

  it should "parse a dataset directory with multiple part files" in {
    import YellowTaxiTrip.helper
    val datasetPath = Paths.get(getClass.getResource("/taxi_sample_dataset").toURI)
    val result = ParquetParser.parseDataset[YellowTaxiTrip](datasetPath)
    result shouldBe a[Success[_]]
  }

  it should "produce 1000 rows total from dataset with multiple parts" in {
    import YellowTaxiTrip.helper
    val datasetPath = Paths.get(getClass.getResource("/taxi_sample_dataset").toURI)
    ParquetParser.parseDataset[YellowTaxiTrip](datasetPath).get.size shouldBe 1000
  }

  it should "fail with clear error when parseParquet receives a directory" in {
    import YellowTaxiTrip.helper
    val datasetPath = Paths.get(getClass.getResource("/taxi_sample_dataset").toURI)
    val result = ParquetParser.parse[YellowTaxiTrip](datasetPath)
    result.failed.get shouldBe a[ParquetParserException]
    result.failed.get.getMessage should include("directory")
  }

  it should "fail with clear error when parseDataset receives a single file" in {
    import YellowTaxiTrip.helper
    val result = ParquetParser.parseDataset[YellowTaxiTrip](samplePath)
    result.failed.get shouldBe a[ParquetParserException]
    result.failed.get.getMessage should include("not a directory")
  }

  it should "analyze a dataset directory without materializing rows" in {
    import com.phasmidsoftware.tableparser.core.table.Analysis
    val datasetPath = Paths.get(getClass.getResource("/taxi_sample_dataset").toURI)
    val stats = Analysis.forParquetDataset[YellowTaxiTrip](datasetPath)
    stats.rows shouldBe 1000
    stats.columns shouldBe 19
  }

  it should "have correct schema for dataset analysis" in {
    import com.phasmidsoftware.tableparser.core.table.Analysis
    val datasetPath = Paths.get(getClass.getResource("/taxi_sample_dataset").toURI)
    val stats = Analysis.forParquetDataset[YellowTaxiTrip](datasetPath)
    stats.columnMap.get("trip_distance").map(_.clazz) shouldBe Some("Double")
    stats.columnMap.get("passenger_count").map(_.clazz) shouldBe Some("Int")
  }

  behavior of "Parquet to CSV"

  it should "write taxi data to CSV" in {
    import YellowTaxiTrip.helper
    val datasetPath = Paths.get(getClass.getResource("/taxi_sample_dataset").toURI)
    val outputPath = Path.of("yellowtaxitrip.csv")
    for {
      t <- ParquetParser.parseDataset[YellowTaxiTrip](datasetPath)
    } t.writeCSVPath(outputPath)
  }
}