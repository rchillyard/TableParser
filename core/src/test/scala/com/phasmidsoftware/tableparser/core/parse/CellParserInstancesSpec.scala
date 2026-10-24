package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.table.{Header, Row}
import java.io.File
import java.net.URI
import java.time.LocalDate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success}

/**
 * Tests of the CellParser instances for the basic types (each converts a String via its Parseable).
 */
class CellParserInstancesSpec extends AnyFlatSpec with should.Matchers:

  behavior of "CellParser instances"

  it should "convert a String into each basic type" in:
    summon[CellParser[Boolean]].convertString("true") shouldBe Success(true)
    summon[CellParser[Char]].convertString("x") shouldBe Success('x')
    summon[CellParser[Byte]].convertString("12") shouldBe Success(12.toByte)
    summon[CellParser[Short]].convertString("1234") shouldBe Success(1234.toShort)
    summon[CellParser[Int]].convertString("123456") shouldBe Success(123456)
    summon[CellParser[Long]].convertString("12345678901") shouldBe Success(12345678901L)
    summon[CellParser[BigInt]].convertString("123456789012345678901") shouldBe Success(BigInt("123456789012345678901"))
    summon[CellParser[Float]].convertString("1.5") shouldBe Success(1.5f)
    summon[CellParser[Double]].convertString("2.25") shouldBe Success(2.25)
    summon[CellParser[BigDecimal]].convertString("3.14159") shouldBe Success(BigDecimal("3.14159"))
    summon[CellParser[String]].convertString("hello") shouldBe Success("hello")
    summon[CellParser[LocalDate]].convertString("2026-10-24") shouldBe Success(LocalDate.of(2026, 10, 24))
    summon[CellParser[java.net.URL]].convertString("https://example.com") shouldBe Success(URI.create("https://example.com").toURL)
    summon[CellParser[File]].convertString("build.sbt") shouldBe Success(new File("build.sbt"))

  it should "fail to convert an invalid String" in:
    summon[CellParser[Int]].convertString("x") should matchPattern { case Failure(_) => }
    summon[CellParser[Char]].convertString("") should matchPattern { case Failure(_) => }
    summon[CellParser[Byte]].convertString("1000") should matchPattern { case Failure(_) => }
    summon[CellParser[LocalDate]].convertString("24 October") should matchPattern { case Failure(_) => }

  it should "take the first character of a String as a Char (as documented in Parseable)" in:
    summon[CellParser[Char]].convertString("xy") shouldBe Success('x')

  it should "describe itself" in:
    summon[CellParser[Int]].toString shouldBe "IntCellParser"
    summon[CellParser[Char]].toString shouldBe "CharCellParser"
    summon[CellParser[File]].toString shouldBe "FileParser"

  it should "not parse a row with a SingleCellParser, nor convert a String with a MultiCellParser" in:
    val header = Header.create("a")
    summon[CellParser[Int]].parse(None, Row(Seq("1"), header, 0), header) should matchPattern { case Failure(_: UnsupportedOperationException) => }
    val multi = new MultiCellParser[Int] {
      def parse(wo: Option[String], row: Row, columns: Header): scala.util.Try[Int] = Success(0)
    }
    multi.convertString("1") should matchPattern { case Failure(_: UnsupportedOperationException) => }
    multi.toString shouldBe "MultiCellParser"
