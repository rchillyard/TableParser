package com.phasmidsoftware.tableparser.kafka.zio

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

object ZIOKafkaBrokerSpec:
  case class Point(x: Int, y: Int) derives CsvProduct

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
