package com.phasmidsoftware.tableparser.kafka.zio

import com.phasmidsoftware.tableparser.core.parse.{CellParser, CellParsers, HeadedCSVTableParser, StandardRowParser, StringParser}
import com.phasmidsoftware.tableparser.core.render.CsvProduct
import com.phasmidsoftware.tableparser.zio.util.EvaluateZIO
import io.github.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import java.nio.charset.StandardCharsets
import java.time.Duration
import org.apache.kafka.clients.consumer.{ConsumerRecord, KafkaConsumer}
import org.apache.kafka.common.serialization.StringDeserializer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.jdk.CollectionConverters._
import scala.util.Using
import zio.ZLayer
import zio.kafka.consumer.{Consumer, ConsumerSettings}
import zio.kafka.producer.{Producer, ProducerSettings}
import zio.kafka.serde.Serde
import zio.stream.ZStream

/**
 * Integration test of ZIOKafka: it sends rows to a Kafka broker (started in this JVM), and reads them back.
 */
class ZIOKafkaBrokerSpec extends AnyFlatSpec with should.Matchers:

  import ZIOKafkaBrokerSpec._

  behavior of "ZIOKafka.toKafka (with a broker)"

  it should "send rows which a consumer can read back, with the column names" in:
    EmbeddedKafka.withRunningKafkaOnFoundPort(EmbeddedKafkaConfig(kafkaPort = 0, controllerPort = 0)) { config =>
      val servers = s"localhost:${config.kafkaPort}"
      val producer = ZLayer.scoped(Producer.make(ProducerSettings(List(servers))))
      val sent = ZStream.fromIterable(points).via(ZIOKafka.toKafka("points", Serde.string)(p => s"${p.x}")).runDrain.provideLayer(producer)
      EvaluateZIO.run(sent) shouldBe Right(())
      val received = consume(servers, "points", points.size)
      received.map(r => (r.key(), r.value())) shouldBe List(("1", "1,2"), ("3", "3,4"), ("5", "5,6"))
      received.map(r => new String(r.headers().lastHeader(ZIOKafka.columnsHeader).value(), StandardCharsets.UTF_8)) shouldBe List("x,y", "x,y", "x,y")
    }

  behavior of "ZIOKafka.fromKafka (with a broker)"

  it should "read back the rows which toKafka sent; and, with fromKafkaCommittable, commit their offsets" in:
    EmbeddedKafka.withRunningKafkaOnFoundPort(EmbeddedKafkaConfig(kafkaPort = 0, controllerPort = 0)) { config =>
      val servers = s"localhost:${config.kafkaPort}"
      val producer = ZLayer.scoped(Producer.make(ProducerSettings(List(servers))))
      def send(ps: List[Point]) = EvaluateZIO.run(ZStream.fromIterable(ps).via(ZIOKafka.toKafka("round", Serde.string)(p => s"${p.x}")).runDrain.provideLayer(producer))
      def consumer(group: String) = ZLayer.scoped(Consumer.make(ConsumerSettings(List(servers)).withGroupId(group).withOffsetRetrieval(Consumer.OffsetRetrieval.Auto(Consumer.AutoOffsetStrategy.Earliest))))
      send(points) shouldBe Right(())
      EvaluateZIO.run(ZIOKafka.fromKafka("round", Serde.string)(PointTableParser).take(3).runCollect.provideLayer(consumer("reader"))).map(_.toList) shouldBe Right(points)
      // With fromKafkaCommittable, each row's offset is committed once it has been processed: the same group then reads only what comes next.
      EvaluateZIO.run(ZIOKafka.fromKafkaCommittable("round", Serde.string)(PointTableParser).take(3).mapZIO((p, o) => o.commit.as(p)).runCollect.provideLayer(consumer("committer"))).map(_.toList) shouldBe Right(points)
      send(List(Point(7, 8))) shouldBe Right(())
      EvaluateZIO.run(ZIOKafka.fromKafkaCommittable("round", Serde.string)(PointTableParser).take(1).map(_._1).runCollect.provideLayer(consumer("committer"))).map(_.toList) shouldBe Right(List(Point(7, 8)))
    }

object ZIOKafkaBrokerSpec:
  case class Point(x: Int, y: Int) derives CsvProduct

  object Point extends CellParsers:
    given CellParser[Point] = CellParser.derived[Point]
    given StringParser[Point] = StandardRowParser.create[Point]

  object PointTableParser extends HeadedCSVTableParser[Point]

  val points: List[Point] = List(Point(1, 2), Point(3, 4), Point(5, 6))

  // Read n records from the start of the topic (or as many as arrive within ten seconds).
  def consume(servers: String, topic: String, n: Int): List[ConsumerRecord[String, String]] =
    val properties: Map[String, AnyRef] = Map("bootstrap.servers" -> servers, "group.id" -> "ZIOKafkaBrokerSpec", "auto.offset.reset" -> "earliest")
    Using.resource(new KafkaConsumer[String, String](properties.asJava, new StringDeserializer, new StringDeserializer)) { consumer =>
      consumer.subscribe(List(topic).asJava)
      val deadline = System.currentTimeMillis() + 10000
      LazyList.continually(consumer.poll(Duration.ofMillis(500)).asScala.toList)
        .scanLeft(List.empty[ConsumerRecord[String, String]])(_ ++ _)
        .find(rs => rs.size >= n || System.currentTimeMillis() > deadline)
        .getOrElse(Nil)
    }
