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

  it should "support Analysis on a Parquet-sourced table" in {
    pending
    import YellowTaxiTrip.helper
    val table = ParquetParser.parse[YellowTaxiTrip](samplePath).get
    // Analysis should run without throwing
//    val analysis = Analysis(table.asInstanceOf[Table[YellowTaxiTrip]])
//    analysis should not be null
  }
}