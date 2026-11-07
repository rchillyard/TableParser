package com.phasmidsoftware.tableparser.kafka.zio

import com.phasmidsoftware.tableparser.core.parse.{CellParser, CellParsers, HeadedCSVTableParser, StandardRowParser, StringParser}
import com.phasmidsoftware.tableparser.core.render.CsvProduct
import com.phasmidsoftware.tableparser.zio.table.ZIOTable
import com.phasmidsoftware.tableparser.zio.util.EvaluateZIO
import java.nio.charset.StandardCharsets
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.{MockProducer, RecordMetadata}
import org.apache.kafka.common.serialization.ByteArraySerializer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.jdk.CollectionConverters._
import zio.kafka.consumer.{Consumer, ConsumerSettings}
import zio.kafka.producer.{Producer, ProducerSettings}
import zio.kafka.serde.Serde
import zio.stream.{ZPipeline, ZStream}
import zio.{Chunk, Semaphore, ZLayer}

/**
 * Tests of ZIOKafka, with the Kafka client's MockProducer (so no broker is needed).
 */
class ZIOKafkaSpec extends AnyFlatSpec with should.Matchers:

  import ZIOKafkaSpec._

  behavior of "ZIOKafka.toKafka"

  it should "send each row as a CSV message, with its key and the column names" in:
    val mock = mockProducer()
    val results = send(mock)(ZStream.fromIterable(points), ZIOKafka.toKafka("points", Serde.string)(p => s"${p.x}"))
    results.map(_.topic()) shouldBe Chunk("points", "points", "points")
    val sent = mock.history().asScala.toList
    sent.map(_.topic()) shouldBe List("points", "points", "points")
    sent.map(r => string(r.key())) shouldBe List("1", "3", "5")
    sent.map(r => string(r.value())) shouldBe List("1,2", "3,4", "5,6")
    sent.map(r => string(r.headers().lastHeader(ZIOKafka.columnsHeader).value())) shouldBe List("x,y", "x,y", "x,y")

  it should "send messages without a key with unitSerializer" in:
    val mock = mockProducer()
    send(mock)(ZStream.fromIterable(points), ZIOKafka.toKafka("points", ZIOKafka.unitSerializer)(_ => ()))
    mock.history().asScala.toList.map(_.key()) shouldBe List(null, null, null)

  it should "send the rows of a stream as they are parsed" in:
    val mock = mockProducer()
    val lines = Seq("x,y", "1,2", "3,4")
    send(mock)(ZIOTable.streamLines(PointTableParser)(lines), ZIOKafka.toKafka("points", ZIOKafka.unitSerializer)(_ => ()))
    mock.history().asScala.toList.map(r => string(r.value())) shouldBe List("1,2", "3,4")

  it should "send nothing for no rows" in:
    val mock = mockProducer()
    send(mock)(ZStream.empty, ZIOKafka.toKafka[Unit, Point]("points", ZIOKafka.unitSerializer)(_ => ())) shouldBe Chunk.empty
    mock.history().size() shouldBe 0

  it should "fail when Kafka refuses a message" in:
    val mock = mockProducer()
    mock.sendException = new RuntimeException("refused")
    val result = EvaluateZIO.run(ZStream.fromIterable(points).via(ZIOKafka.toKafka("points", ZIOKafka.unitSerializer)(_ => ())).runCollect.provideLayer(producer(mock)))
    result.left.map(_.getMessage) shouldBe Left("refused")

  behavior of "ZIOKafka.toKafkaWith"

  it should "send each row rendered by the given function, with no headers" in:
    val mock = mockProducer()
    def json(p: Point): String = s"""{"x":${p.x},"y":${p.y}}"""
    send(mock)(ZStream.fromIterable(points), ZIOKafka.toKafkaWith("points", ZIOKafka.unitSerializer)(_ => (), json))
    val sent = mock.history().asScala.toList
    sent.map(r => string(r.value())) shouldBe List("""{"x":1,"y":2}""", """{"x":3,"y":4}""", """{"x":5,"y":6}""")
    sent.map(_.headers().toArray.length) shouldBe List(0, 0, 0)

  behavior of "reading messages back (as in the README)"

  it should "parse a CSV message with its header, using the parser which read the rows" in:
    val mock = mockProducer()
    send(mock)(ZStream.fromIterable(points), ZIOKafka.toKafka("points", ZIOKafka.unitSerializer)(_ => ()))
    val sent = mock.history().asScala.toList
    val header = string(sent.head.headers().lastHeader(ZIOKafka.columnsHeader).value())
    val parsed = EvaluateZIO.run(ZIOTable.streamLines(PointTableParser)(Seq(header) ++ sent.map(r => string(r.value()))).runCollect)
    parsed.map(_.toList) shouldBe Right(points)

  behavior of "ZIOKafka.csvRecords"

  it should "make a record for each row, without sending it" in:
    val records = EvaluateZIO.run(ZStream.fromIterable(points).via(ZIOKafka.csvRecords("points")(_.x)).runCollect).fold(e => throw e, _.toList)
    records.map(r => (r.topic(), r.key(), r.value())) shouldBe List(("points", 1, "1,2"), ("points", 3, "3,4"), ("points", 5, "5,6"))
    records.map(r => string(r.headers().lastHeader(ZIOKafka.columnsHeader).value())) shouldBe List("x,y", "x,y", "x,y")

  behavior of "ZIOKafka.parseRecord (as fromKafka reads each message)"

  it should "parse a message's value with the column names in its header" in:
    val record = new ConsumerRecord[String, String]("points", 0, 0L, "3", "4,3")
    record.headers().add(ZIOKafka.columnsHeader, "y,x".getBytes(StandardCharsets.UTF_8))
    ZIOKafka.parseRecord(PointTableParser)(record) shouldBe scala.util.Success(Some(Point(3, 4)))

  it should "fail for a message without the header (when the parser has no fixed header)" in:
    ZIOKafka.parseRecord(PointTableParser)(new ConsumerRecord[String, String]("points", 0, 0L, "3", "3,4")).isFailure shouldBe true

  behavior of "ZIOKafka.fromKafka (with a MockConsumer)"

  it should "read rows back from a topic, and commit each chunk's offsets once its rows have been emitted" in:
    val mock = MockKafka.consumer("points", Seq(Seq("x,y" -> "1,2", "x,y" -> "3,4", "y,x" -> "6,5"), Seq("x,y" -> "7,8")))
    EvaluateZIO.run(ZIOKafka.fromKafka("points", Serde.string)(PointTableParser).take(4).runCollect.provideLayer(consumer(mock))).map(_.toList) shouldBe Right(List(Point(1, 2), Point(3, 4), Point(5, 6), Point(7, 8)))
    // The first chunk (offsets 0 to 2) was committed before the fourth row was read: the next offset to read is 3.
    mock.committedOffsets should contain(3L)

  it should "give each row with its offset, for the caller to commit" in:
    val mock = MockKafka.consumer("points", Seq(Seq("x,y" -> "1,2", "x,y" -> "3,4")))
    EvaluateZIO.run(ZIOKafka.fromKafkaCommittable("points", Serde.string)(PointTableParser).take(1).mapZIO((p, o) => o.commit.as(p)).runCollect.provideLayer(consumer(mock))).map(_.toList) shouldBe Right(List(Point(1, 2)))
    mock.committedOffsets shouldBe List(1L)

  it should "decode each message with a function (fromKafkaWith)" in:
    val mock = MockKafka.consumer("numbers", Seq(Seq("" -> "1", "" -> "2")))
    EvaluateZIO.run(ZIOKafka.fromKafkaWith("numbers", Serde.string)(w => scala.util.Try(w.toInt)).take(2).runCollect.provideLayer(consumer(mock))).map(_.toList) shouldBe Right(List(1, 2))

  it should "fail when a message can't be parsed (unless the parser is forgiving)" in:
    val mock = MockKafka.consumer("points", Seq(Seq("x,y" -> "1,oops")))
    EvaluateZIO.run(ZIOKafka.fromKafka("points", Serde.string)(PointTableParser).take(1).runCollect.provideLayer(consumer(mock))).isLeft shouldBe true

object ZIOKafkaSpec:
  case class Point(x: Int, y: Int) derives CsvProduct

  object Point extends CellParsers:
    given CellParser[Point] = CellParser.derived[Point]

  given StringParser[Point] = StandardRowParser.create[Point]

  object PointTableParser extends HeadedCSVTableParser[Point]

  val points: List[Point] = List(Point(1, 2), Point(3, 4), Point(5, 6))

  // A MockProducer which acknowledges each message as soon as it is sent.
  def mockProducer(): MockProducer[Array[Byte], Array[Byte]] =
    new MockProducer[Array[Byte], Array[Byte]](true, null, new ByteArraySerializer, new ByteArraySerializer)

  def producer(mock: MockProducer[Array[Byte], Array[Byte]]): ZLayer[Any, Throwable, Producer] =
    ZLayer.scoped(Producer.fromJavaProducer(mock, ProducerSettings()))

  def send[X](mock: MockProducer[Array[Byte], Array[Byte]])(xs: ZStream[Any, Throwable, X], pipeline: ZPipeline[Producer, Throwable, X, RecordMetadata]): Chunk[RecordMetadata] =
    EvaluateZIO.run(xs.via(pipeline).runCollect.provideLayer(producer(mock))).fold(e => throw e, identity)

  def string(bytes: Array[Byte]): String = new String(bytes, StandardCharsets.UTF_8)

  // A Consumer which uses the given MockConsumer (the bootstrap servers are never contacted).
  def consumer(mock: MockKafka.RecordingConsumer): ZLayer[Any, Throwable, Consumer] =
    val settings = ConsumerSettings(List("localhost:9092")).withGroupId("ZIOKafkaSpec").withOffsetRetrieval(Consumer.OffsetRetrieval.Auto(Consumer.AutoOffsetStrategy.Earliest))
    ZLayer.scoped(Semaphore.make(1).flatMap(access => Consumer.fromJavaConsumerWithPermit(mock, settings, access)))
