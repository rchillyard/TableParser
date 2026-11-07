package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.render.{CsvGenerator, CsvProduct, CsvRenderer}
import com.phasmidsoftware.tableparser.core.table.{Header, Table}
import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, ZoneId, ZoneOffset, ZonedDateTime}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source
import scala.util.{Failure, Success, Try}

/**
 * Tests of the java.time cell parsers and renderers (#118): a table with a field of each type is written and read back.
 */
class JavaTimeSpec extends AnyFlatSpec with Matchers:

  import JavaTimeSpec._

  behavior of "java.time cells"

  it should "parse each type from its ISO-8601 form" in:
    summon[CellParser[Instant]].asInstanceOf[SingleCellParser[Instant]].convertString("2024-01-01T00:34:44Z") shouldBe Success(Instant.parse("2024-01-01T00:34:44Z"))
    summon[Parseable[LocalDateTime]].parse("2024-01-01T00:34:44") shouldBe Success(LocalDateTime.of(2024, 1, 1, 0, 34, 44))
    summon[Parseable[LocalTime]].parse("00:34:44") shouldBe Success(LocalTime.of(0, 34, 44))
    summon[Parseable[OffsetDateTime]].parse("2024-01-01T00:34:44-05:00") shouldBe Success(OffsetDateTime.of(2024, 1, 1, 0, 34, 44, 0, ZoneOffset.ofHours(-5)))
    summon[Parseable[ZonedDateTime]].parse("2024-01-01T00:34:44-05:00[America/New_York]") shouldBe Success(ZonedDateTime.of(2024, 1, 1, 0, 34, 44, 0, ZoneId.of("America/New_York")))

  it should "name its parsers (for debugging)" in:
    summon[CellParser[Instant]].toString shouldBe "InstantParser"
    summon[CellParser[ZonedDateTime]].toString shouldBe "ZonedDateTimeParser"

  it should "fail to parse a malformed value, with the type's name" in:
    summon[Parseable[Instant]].parse("yesterday") match
      case Failure(x) => x.getMessage should include("Instant")
      case x => fail(s"expected a failure, not $x")

  it should "parse an empty optional value as None" in:
    summon[CellParser[Option[Instant]]].asInstanceOf[SingleCellParser[Option[Instant]]].convertString("") shouldBe Success(None)
    summon[CellParser[Option[ZonedDateTime]]].asInstanceOf[SingleCellParser[Option[ZonedDateTime]]].convertString("") shouldBe Success(None)

  it should "render each type in its ISO-8601 form" in:
    summon[CsvRenderer[LocalDateTime]].render(LocalDateTime.of(2024, 1, 1, 0, 34, 44), Map()) shouldBe "2024-01-01T00:34:44"
    summon[CsvRenderer[Option[LocalDate]]].render(None, Map()) shouldBe ""
    summon[CsvRenderer[Option[LocalDate]]].render(Some(LocalDate.of(2024, 1, 1)), Map()) shouldBe "2024-01-01"

  it should "write a table with a field of each type, and read it back" in:
    val result = for
      csv <- Table(events, Some(Header.create(CsvGenerator.header[Event].split(",").toSeq*))).toCSV
      again <- Table.parse[Table[Event]](Try(Source.fromString(csv)))
    yield again.toSeq
    result shouldBe Success(events)

object JavaTimeSpec:
  case class Event(
      name: String
    , at: Instant
    , local: LocalDateTime
    , time: LocalTime
    , offset: OffsetDateTime
    , zoned: ZonedDateTime
    , maybeAt: Option[Instant]
    , maybeLocal: Option[LocalDateTime]
    , maybeTime: Option[LocalTime]
    , maybeOffset: Option[OffsetDateTime]
    , maybeZoned: Option[ZonedDateTime]
    , maybeDate: Option[LocalDate]
  ) derives CsvProduct

  object Event extends CellParsers:
    given CellParser[Event] = CellParser.derived[Event]
    given StringParser[Event] = StandardRowParser.create[Event]

    given EventTableParser: HeadedCSVTableParser[Event] = new HeadedCSVTableParser[Event] {}

  private val newYork = ZoneId.of("America/New_York")

  val events: Seq[Event] = Seq(
      Event("first", Instant.parse("2024-01-01T00:34:44Z"), LocalDateTime.of(2024, 1, 1, 0, 34, 44), LocalTime.of(0, 34, 44)
        , OffsetDateTime.of(2024, 1, 1, 0, 34, 44, 0, ZoneOffset.ofHours(-5)), ZonedDateTime.of(2024, 1, 1, 0, 34, 44, 0, newYork)
        , Some(Instant.parse("2024-06-30T12:00:00.123Z")), Some(LocalDateTime.of(2024, 6, 30, 12, 0)), Some(LocalTime.of(23, 59, 59, 999_000_000))
        , Some(OffsetDateTime.of(2024, 6, 30, 12, 0, 0, 0, ZoneOffset.UTC)), Some(ZonedDateTime.of(2024, 6, 30, 12, 0, 0, 0, newYork)), Some(LocalDate.of(2024, 6, 30)))
    , Event("second", Instant.EPOCH, LocalDateTime.of(1970, 1, 1, 0, 0), LocalTime.MIDNIGHT
        , OffsetDateTime.of(1970, 1, 1, 0, 0, 0, 0, ZoneOffset.UTC), ZonedDateTime.of(1970, 1, 1, 0, 0, 0, 0, ZoneId.of("UTC"))
        , None, None, None, None, None, None)
  )
