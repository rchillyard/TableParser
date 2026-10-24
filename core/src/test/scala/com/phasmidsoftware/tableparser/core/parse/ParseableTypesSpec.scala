package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.table.{Analysis, Statistics}
import java.io.File
import java.nio.file.{Files, Paths}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success}

/**
 * Tests of the less common Parseable types, and of CSV analysis from a file.
 */
class ParseableTypesSpec extends AnyFlatSpec with should.Matchers:

  behavior of "Parseable"

  it should "parse Char, Short, Float and BigDecimal" in:
    summon[Parseable[Char]].parse("z") shouldBe Success('z')
    summon[Parseable[Short]].parse("-123") shouldBe Success((-123).toShort)
    summon[Parseable[Float]].parse("2.5") shouldBe Success(2.5f)
    summon[Parseable[BigDecimal]].parse("1.000000000000000000001") shouldBe Success(BigDecimal("1.000000000000000000001"))
    summon[Parseable[Short]].parse("40000") should matchPattern { case Failure(_) => }

  it should "parse a BigInt in base 10 or in the radix given" in:
    summon[Parseable[BigInt]].parse("123456789012345678901234567890") shouldBe Success(BigInt("123456789012345678901234567890"))
    summon[Parseable[BigInt]].parse("ff", Some("16")) shouldBe Success(BigInt(255))
    summon[Parseable[BigInt]].parse("fg", Some("16")) should matchPattern { case Failure(_) => }

  it should "parse a Path and a File" in:
    summon[Parseable[java.nio.file.Path]].parse("a/b.csv") shouldBe Success(Paths.get("a/b.csv"))
    summon[Parseable[File]].parse("a/b.csv") shouldBe Success(new File("a/b.csv"))

  it should "fail to parse an invalid URL" in:
    summon[Parseable[java.net.URL]].parse("not a url") should matchPattern { case Failure(_) => }

  it should "parse a list of strings" in:
    summon[Parseable[StringList]].parse("{a,b,c}") shouldBe Success(List("a", "b", "c"))
    summon[Parseable[StringList]].parse("a") shouldBe Success(List("a"))

  behavior of "Statistics and Analysis"

  it should "make the statistics of no values, one value, or several" in:
    Statistics.make(Nil) shouldBe None
    Statistics.make(List(3.0)) shouldBe Some(Statistics(3.0, 0, 3.0, 3.0))
    Statistics.make(List(1.0, 3.0)).map(_.mu) shouldBe Some(2.0)

  it should "analyze a CSV file" in:
    val file = File.createTempFile("analysis", ".csv")
    file.deleteOnExit()
    Files.writeString(file.toPath, "name,age\nAnn,30\nBob,40\n")
    val stats = Analysis.forCsv(file.toPath)
    stats.rows shouldBe 2
    stats.columns shouldBe 2
    stats.columnMap("age").clazz shouldBe "Int"

  it should "fail to analyze a missing file" in:
    a[TableParserException] should be thrownBy Analysis.forCsv(Paths.get("no/such/file.csv"))
