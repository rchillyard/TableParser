package com.phasmidsoftware.tableparser.parquet

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper.camelToSnakeCaseColumnNameMapper
import com.phasmidsoftware.tableparser.core.parse.{CellParsers, ColumnHelper}
import com.phasmidsoftware.tableparser.core.table.Table
import java.nio.file.{Path, Paths}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Success, Try}

class ParquetParserSpec extends AnyFlatSpec with Matchers with CellParsers {

  // Path to the committed test fixture
  private val samplePath: Path =
    Paths.get(getClass.getResource("/taxi_sample.parquet").toURI)

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
    import YellowTaxiTrip.helper
    val result = ParquetParser.parse[YellowTaxiTrip](samplePath)
    result.get.size shouldBe 1000
  }

  it should "parse typed field values correctly for the first row" in {
    import YellowTaxiTrip.helper
    val firstRow = ParquetParser.parse[YellowTaxiTrip](samplePath).get.head
    // vendorId should be 1 or 2 (Creative Mobile Technologies or VeriFone)
    firstRow.storeAndFwdFlag should (be(Some("Y")) or be(Some("N")))
    // tripDistance should be non-negative
    firstRow.tripDistance.get should be >= 0.0
    // fareAmount should be non-negative
    firstRow.fareAmount.get should be >= 0.0
    // pickup should be before dropoff
    firstRow.tpepPickupDatetime.zip(firstRow.tpepDropoffDatetime).headOption.forall { case (p, d) => p.isBefore(d) } shouldBe true
    // storeAndFwdFlag should be Y or N
    firstRow.storeAndFwdFlag should (be(Some("Y")) or be(Some("N")))
  }

  it should "have a header with 19 columns" in {
    import YellowTaxiTrip.helper
    val table = ParquetParser.parse[YellowTaxiTrip](samplePath).get
    table.maybeHeader.map(_.size) shouldBe Some(19)
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

  // ── Dataset (multi-file) ───────────────────────────────────────────────────

  it should "parse a two-part dataset directory" in {
    pending
    // ParquetReader requires directory support -- to be implemented
    import YellowTaxiTrip.helper
    // This test requires taxi_sample_dataset/ with two part files
    // generated from taxi_sample.parquet -- see design document section 9.
    val datasetPath = Paths.get(getClass.getResource("/taxi_sample_dataset").toURI)
    val result = ParquetParser.parse[YellowTaxiTrip](datasetPath)
    result shouldBe a[Success[_]]
    result.get.size shouldBe 1000
  }

  // ── Analysis ──────────────────────────────────────────────────────────────

  behavior of "Analysis"

// ── Analysis ──────────────────────────────────────────────────────────

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
    val maybeStats = col.maybeStatistics
    maybeStats match {
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
    val storeAndFwdStats = Column.statisticsFrom(samplePath, "store_and_fwd_flag")
    storeAndFwdStats shouldBe None
  }

  it should "return None for non-existent columns when computing statistics" in {
    import com.phasmidsoftware.tableparser.core.table.Column
    val nonExistentStats = Column.statisticsFrom(samplePath, "nonexistent_column")
    nonExistentStats shouldBe None
  }

  it should "support metadata-only mode (return None if no metadata)" in {
    import com.phasmidsoftware.tableparser.core.table.Column
    // useMetadataOnly=true (default) should return None if metadata unavailable
    val metadataOnlyStats = Column.statisticsFrom(samplePath, "trip_distance", useMetadataOnly = true)
    // Currently no metadata extraction implemented, so should return None
    metadataOnlyStats shouldBe None
  }

  it should "support lazy fallback mode (return LazyStatistics if no metadata)" in {
    import com.phasmidsoftware.tableparser.core.table.{Column, LazyStatistics}
    // useMetadataOnly=false should return LazyStatistics even if metadata unavailable
    val lazyStats = Column.statisticsFrom(samplePath, "trip_distance", useMetadataOnly = false)
    lazyStats shouldBe a[Some[_]]
    val col = lazyStats.get
    col.maybeStatistics match {
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
    val result = ParquetParser.parseDataset[YellowTaxiTrip](datasetPath)
    result.get.size shouldBe 1000
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
    val filePath = samplePath
    val result = ParquetParser.parseDataset[YellowTaxiTrip](filePath)
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
}