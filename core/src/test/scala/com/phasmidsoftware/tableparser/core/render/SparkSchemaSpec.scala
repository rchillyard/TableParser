package com.phasmidsoftware.tableparser.core.render

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper.camelToSnakeCaseColumnNameMapperLower
import com.phasmidsoftware.tableparser.core.parse.{CellParsers, ColumnHelper}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZonedDateTime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Success

/**
 * Tests of SparkSchema (#117): the Spark DDL schema of a row type's CSV form, matching CsvGenerator.header.
 */
class SparkSchemaSpec extends AnyFlatSpec with Matchers:

  import SparkSchemaSpec._

  behavior of "SparkType"

  it should "give the Spark SQL type of each field type" in:
    def sql[T](using t: SparkType[T]): String = t.sqlType
    sql[String] shouldBe "STRING"
    sql[Boolean] shouldBe "BOOLEAN"
    sql[Byte] shouldBe "TINYINT"
    sql[Short] shouldBe "SMALLINT"
    sql[Int] shouldBe "INT"
    sql[Long] shouldBe "BIGINT"
    sql[Float] shouldBe "FLOAT"
    sql[Double] shouldBe "DOUBLE"
    sql[BigInt] shouldBe "DECIMAL(38,0)"
    sql[BigDecimal] shouldBe "DECIMAL(38,18)"
    sql[LocalDate] shouldBe "DATE"
    sql[Instant] shouldBe "TIMESTAMP"
    sql[LocalDateTime] shouldBe "TIMESTAMP_NTZ"
    sql[OffsetDateTime] shouldBe "TIMESTAMP"
    sql[ZonedDateTime] shouldBe "STRING"
    sql[LocalTime] shouldBe "STRING"
    sql[java.net.URL] shouldBe "STRING"
    sql[Option[Long]] shouldBe "BIGINT"

  behavior of "SparkSchema.ddl"

  it should "give the schema of KAFKA.md's taxi trips, with the column names of the ColumnHelper" in:
    CsvGenerator.header[Trip] shouldBe "VendorID,tpep_pickup_datetime,tpep_dropoff_datetime,passenger_count,trip_distance,fare_amount,tip_amount,total_amount"
    SparkSchema.ddl[Trip] shouldBe Success("`VendorID` INT, `tpep_pickup_datetime` TIMESTAMP, `tpep_dropoff_datetime` TIMESTAMP, `passenger_count` INT, `trip_distance` DOUBLE, `fare_amount` DOUBLE, `tip_amount` DOUBLE, `total_amount` DOUBLE")

  it should "flatten a nested case class, as CsvGenerator does, and give an Option the type of its value" in:
    // Without a ColumnHelper prefix, the columns of a nested case class are named by its own fields.
    CsvGenerator.header[Person] shouldBe "name,born,street,zip,score"
    SparkSchema.ddl[Person] shouldBe Success("`name` STRING, `born` DATE, `street` STRING, `zip` INT, `score` DOUBLE")

  it should "quote a column name which needs it" in:
    SparkSchema.ddl[Odd] shouldBe Success("`a, b` BIGINT, `c``d` BOOLEAN")

  it should "fail if the column names and the types don't match" in:
    given SparkSchema[Trip] = SparkSchema[Trip](List("INT"))
    SparkSchema.ddl[Trip].isFailure shouldBe true

object SparkSchemaSpec:
  case class Trip(vendorId: Int, pickup: Instant, dropoff: Instant, passengerCount: Int, tripDistance: Double, fareAmount: Double, tipAmount: Double, totalAmount: Double) derives CsvProduct, SparkSchema

  object Trip extends CellParsers:
    given ColumnHelper[Trip] = columnHelper(
      camelToSnakeCaseColumnNameMapperLower
      , "vendorId" -> "VendorID"
      , "pickup" -> "tpep_pickup_datetime"
      , "dropoff" -> "tpep_dropoff_datetime"
    )

  case class Address(street: String, zip: Int)

  case class Person(name: String, born: LocalDate, address: Address, score: Option[Double]) derives CsvProduct, SparkSchema

  case class Odd(ab: Long, cd: Boolean) derives CsvProduct, SparkSchema

  object Odd extends CellParsers:
    given ColumnHelper[Odd] = columnHelper("ab" -> "a, b", "cd" -> "c`d")
