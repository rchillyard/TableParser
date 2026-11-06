package com.phasmidsoftware.tableparser.kafka.cats

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.tableparser.cats.table.IOTable
import com.phasmidsoftware.tableparser.core.parse.{CellParser, CellParsers, HeadedCSVTableParser, StandardRowParser, StringParser}
import com.phasmidsoftware.tableparser.core.render.CsvProduct
import fs2.Stream
import fs2.kafka.producer.MkProducer
import fs2.kafka.{KafkaByteProducer, ProducerSettings}
import java.nio.charset.StandardCharsets
import org.apache.kafka.clients.producer.MockProducer
import org.apache.kafka.common.serialization.ByteArraySerializer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.jdk.CollectionConverters._

/**
 * Tests of IOKafka, with the Kafka client's MockProducer (so no broker is needed).
 */
class IOKafkaSpec extends AnyFlatSpec with should.Matchers:

  import IOKafkaSpec._

  behavior of "IOKafka.toKafka"

  it should "send each row as a CSV message, with its key and the column names" in:
    val mock = mockProducer()
    given MkProducer[IO] = mkProducer(mock)
    val results = Stream.emits(points).through(IOKafka.toKafka("points", stringSettings)(p => s"${p.x}")).compile.toList.unsafeRunSync()
    results.flatMap(_.toList).size shouldBe 3
    val sent = mock.history().asScala.toList
    sent.map(_.topic()) shouldBe List("points", "points", "points")
    sent.map(r => string(r.key())) shouldBe List("1", "3", "5")
    sent.map(r => string(r.value())) shouldBe List("1,2", "3,4", "5,6")
    sent.map(r => string(r.headers().lastHeader(IOKafka.columnsHeader).value())) shouldBe List("x,y", "x,y", "x,y")

  it should "send messages without a key when the key type is Unit" in:
    val mock = mockProducer()
    given MkProducer[IO] = mkProducer(mock)
    Stream.emits(points).through(IOKafka.toKafka("points", unitSettings)(_ => ())).compile.drain.unsafeRunSync()
    mock.history().asScala.toList.map(_.key()) shouldBe List(null, null, null)

  it should "send the rows of a stream as they are parsed" in:
    val mock = mockProducer()
    given MkProducer[IO] = mkProducer(mock)
    val lines = Seq("x,y", "1,2", "3,4")
    IOTable.streamLines(PointTableParser)(lines).through(IOKafka.toKafka("points", unitSettings)(_ => ())).compile.drain.unsafeRunSync()
    mock.history().asScala.toList.map(r => string(r.value())) shouldBe List("1,2", "3,4")

  it should "send nothing for no rows" in:
    val mock = mockProducer()
    given MkProducer[IO] = mkProducer(mock)
    Stream.empty.covaryAll[IO, Point].through(IOKafka.toKafka("points", unitSettings)(_ => ())).compile.drain.unsafeRunSync()
    mock.history().size() shouldBe 0

  it should "fail when Kafka refuses a message" in:
    val mock = mockProducer()
    mock.sendException = new RuntimeException("refused")
    given MkProducer[IO] = mkProducer(mock)
    val result = Stream.emits(points).through(IOKafka.toKafka("points", unitSettings)(_ => ())).compile.drain.attempt.unsafeRunSync()
    result.left.map(_.getMessage) shouldBe Left("refused")

  behavior of "IOKafka.toKafkaWith"

  it should "send each row rendered by the given function, with no headers" in:
    val mock = mockProducer()
    given MkProducer[IO] = mkProducer(mock)
    def json(p: Point): String = s"""{"x":${p.x},"y":${p.y}}"""
    Stream.emits(points).through(IOKafka.toKafkaWith("points", unitSettings)(_ => (), json)).compile.drain.unsafeRunSync()
    val sent = mock.history().asScala.toList
    sent.map(r => string(r.value())) shouldBe List("""{"x":1,"y":2}""", """{"x":3,"y":4}""", """{"x":5,"y":6}""")
    sent.map(_.headers().toArray.length) shouldBe List(0, 0, 0)

  it should "send each row as JSON, with a circe Encoder (as in the README)" in:
    import io.circe.syntax.*
    val mock = mockProducer()
    given MkProducer[IO] = mkProducer(mock)
    Stream.emits(jsonPoints).through(IOKafka.toKafkaWith("points", stringSettings)(p => s"${p.x}", _.asJson.noSpaces)).compile.drain.unsafeRunSync()
    mock.history().asScala.toList.map(r => string(r.value())) shouldBe List("""{"x":1,"y":2}""", """{"x":3,"y":4}""")

  behavior of "reading messages back (as in the README)"

  it should "parse a CSV message with its header, using the parser which read the rows" in:
    val mock = mockProducer()
    given MkProducer[IO] = mkProducer(mock)
    Stream.emits(points).through(IOKafka.toKafka("points", unitSettings)(_ => ())).compile.drain.unsafeRunSync()
    val sent = mock.history().asScala.toList
    val parsed = sent.flatMap { record =>
      val header = string(record.headers().lastHeader(IOKafka.columnsHeader).value())
      IOTable.streamLines(PointTableParser)(Seq(header, string(record.value()))).compile.toList.unsafeRunSync()
    }
    parsed shouldBe points
    val header = string(sent.head.headers().lastHeader(IOKafka.columnsHeader).value())
    IOTable.streamLines(PointTableParser)(Seq(header) ++ sent.map(r => string(r.value()))).compile.toList.unsafeRunSync() shouldBe points

  behavior of "IOKafka.csvRecords"

  it should "make a record for each row, without sending it" in:
    val records = Stream.emits(points).through(IOKafka.csvRecords("points")(_.x)).compile.toList.unsafeRunSync()
    records.map(r => (r.topic, r.key, r.value)) shouldBe List(("points", 1, "1,2"), ("points", 3, "3,4"), ("points", 5, "5,6"))
    records.map(_.headers.withKey(IOKafka.columnsHeader).map(_.as[String])) shouldBe List(Some("x,y"), Some("x,y"), Some("x,y"))

object IOKafkaSpec:
  case class Point(x: Int, y: Int) derives CsvProduct

  object Point extends CellParsers:
    given CellParser[Point] = CellParser.derived[Point]

  given StringParser[Point] = StandardRowParser.create[Point]

  object PointTableParser extends HeadedCSVTableParser[Point]

  val points: List[Point] = List(Point(1, 2), Point(3, 4), Point(5, 6))

  case class JsonPoint(x: Int, y: Int) derives io.circe.Encoder.AsObject

  val jsonPoints: List[JsonPoint] = List(JsonPoint(1, 2), JsonPoint(3, 4))

  // The bootstrap servers are never contacted, as MkProducer makes a MockProducer instead.
  val stringSettings: ProducerSettings[IO, String, String] = ProducerSettings[IO, String, String].withBootstrapServers("localhost:9092")
  val unitSettings: ProducerSettings[IO, Unit, String] = ProducerSettings[IO, Unit, String].withBootstrapServers("localhost:9092")

  // A MockProducer which acknowledges each message as soon as it is sent.
  def mockProducer(): MockProducer[Array[Byte], Array[Byte]] =
    new MockProducer[Array[Byte], Array[Byte]](true, null, new ByteArraySerializer, new ByteArraySerializer)

  def mkProducer(mock: MockProducer[Array[Byte], Array[Byte]]): MkProducer[IO] = new MkProducer[IO]:
    def apply[G[_]](settings: ProducerSettings[G, ?, ?]): IO[KafkaByteProducer] = IO.pure(mock)

  def string(bytes: Array[Byte]): String = new String(bytes, StandardCharsets.UTF_8)
