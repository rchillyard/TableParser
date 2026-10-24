package com.phasmidsoftware.tableparser.parquet

import java.time.{Instant, LocalDate}
import org.apache.parquet.schema.LogicalTypeAnnotation.{TimeUnit, dateType, decimalType, stringType, timestampType}
import org.apache.parquet.schema.PrimitiveType.PrimitiveTypeName._
import org.apache.parquet.schema.{PrimitiveType, Types}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ParquetTypeMapperSpec extends AnyFlatSpec with should.Matchers:

  behavior of "ParquetTypeMapper"

  private def required(t: PrimitiveType.PrimitiveTypeName): Types.PrimitiveBuilder[PrimitiveType] = Types.required(t)

  it should "map each supported Parquet type to its Scala type" in:
    ParquetTypeMapper.mapType(required(BOOLEAN).named("a")) shouldBe Right(classOf[Boolean])
    ParquetTypeMapper.mapType(required(INT32).named("a")) shouldBe Right(classOf[Int])
    ParquetTypeMapper.mapType(required(INT32).as(dateType()).named("a")) shouldBe Right(classOf[LocalDate])
    ParquetTypeMapper.mapType(required(INT32).as(decimalType(2, 9)).named("a")) shouldBe Right(classOf[BigDecimal])
    ParquetTypeMapper.mapType(required(INT64).named("a")) shouldBe Right(classOf[Long])
    ParquetTypeMapper.mapType(required(INT64).as(timestampType(true, TimeUnit.MICROS)).named("a")) shouldBe Right(classOf[Instant])
    ParquetTypeMapper.mapType(required(INT64).as(decimalType(2, 18)).named("a")) shouldBe Right(classOf[BigDecimal])
    ParquetTypeMapper.mapType(required(FLOAT).named("a")) shouldBe Right(classOf[Float])
    ParquetTypeMapper.mapType(required(DOUBLE).named("a")) shouldBe Right(classOf[Double])
    ParquetTypeMapper.mapType(required(BINARY).as(stringType()).named("a")) shouldBe Right(classOf[String])
    ParquetTypeMapper.mapType(required(FIXED_LEN_BYTE_ARRAY).length(16).as(decimalType(2, 38)).named("a")) shouldBe Right(classOf[BigDecimal])

  it should "reject an unsupported Parquet type" in:
    ParquetTypeMapper.mapType(required(INT96).named("a")) should matchPattern { case Left(_: ParquetParserException) => }
    ParquetTypeMapper.mapType(required(FIXED_LEN_BYTE_ARRAY).length(16).named("a")) should matchPattern { case Left(_: ParquetParserException) => }

  it should "tell whether a column is optional" in:
    ParquetTypeMapper.isOptional(Types.optional(INT32).named("a")) shouldBe true
    ParquetTypeMapper.isOptional(required(INT32).named("a")) shouldBe false
