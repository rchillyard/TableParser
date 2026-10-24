package com.phasmidsoftware.tableparser.parquet

import java.time.LocalDate
import org.apache.parquet.example.data.simple.SimpleGroup
import org.apache.parquet.io.api.Binary
import org.apache.parquet.schema.LogicalTypeAnnotation.{dateType, decimalType}
import org.apache.parquet.schema.PrimitiveType.PrimitiveTypeName._
import org.apache.parquet.schema.{MessageType, Types}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success, Try}

/**
 * Tests of ParquetCellConverter with an in-memory record (a SimpleGroup).
 */
class ParquetCellConverterSpec extends AnyFlatSpec with should.Matchers:

  import ParquetCellConverterSpec._

  private val schema: MessageType = Types.buildMessage()
          .required(BOOLEAN).named("b")
          .required(FLOAT).named("f")
          .required(INT32).as(dateType()).named("d")
          .required(BINARY).as(decimalType(0, 10)).named("m")
          .optional(INT32).named("o")
          .named("test")

  private val group: SimpleGroup =
    val g = new SimpleGroup(schema)
    g.add("b", true)
    g.add("f", 1.5f)
    g.add("d", LocalDate.of(2026, 10, 24).toEpochDay.toInt)
    g.add("m", Binary.fromConstantByteArray(java.math.BigInteger.valueOf(12345).toByteArray))
    g

  behavior of "ParquetCellConverter"

  it should "convert each primitive type" in:
    summon[ParquetCellConverter[Boolean]].convert(group, "b") shouldBe Success(true)
    summon[ParquetCellConverter[Float]].convert(group, "f") shouldBe Success(1.5f)
    summon[ParquetCellConverter[LocalDate]].convert(group, "d") shouldBe Success(LocalDate.of(2026, 10, 24))
    summon[ParquetCellConverter[BigDecimal]].convert(group, "m") shouldBe Success(BigDecimal(12345))

  it should "convert a missing optional value to None, and a present one to Some" in:
    summon[ParquetCellConverter[Option[Int]]].convert(group, "o") shouldBe Success(None)
    summon[ParquetCellConverter[Option[Boolean]]].convert(group, "b") shouldBe Success(Some(true))

  it should "list the column it reads (by default, of any type)" in:
    val custom = new ParquetCellConverter[String]:
      def convert(group: SimpleGroup, fieldName: String): Try[String] = Success(fieldName)
    custom.columns("field", "column") shouldBe Seq(ParquetColumn("field", "column", None, optional = false))
    summon[ParquetCellConverter[Option[Float]]].columns("field", "column") shouldBe Seq(ParquetColumn("field", "column", Some(classOf[Float]), optional = true))

  it should "derive a converter for a case class" in:
    ParquetCellConverter.derived[Good].convert(group, "") shouldBe Success(Good(b = true, f = 1.5f))

  it should "report which field failed, also from inside a nested case class" in:
    ParquetCellConverter.derived[Bad].convert(group, "") should matchPattern { case Failure(x: ParquetParserException) if x.getMessage.contains("Failed to convert field 'b'") => }
    ParquetCellConverter.derived[Outer].convert(group, "") should matchPattern { case Failure(x: ParquetParserException) if x.getMessage.contains("Failed to convert field 'b'") => }

object ParquetCellConverterSpec:
  case class Good(b: Boolean, f: Float)

  // b is a BOOLEAN column, which cannot be read as an Int.
  case class Bad(b: Int)

  case class Outer(inner: Bad)
