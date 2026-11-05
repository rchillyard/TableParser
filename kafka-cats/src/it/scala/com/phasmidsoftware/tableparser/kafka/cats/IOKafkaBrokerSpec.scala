package com.phasmidsoftware.tableparser.kafka.cats

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.tableparser.core.render.CsvProduct
import fs2.Stream
import fs2.kafka.ProducerSettings
import io.github.embeddedkafka.{EmbeddedKafka, EmbeddedKafkaConfig}
import java.nio.charset.StandardCharsets
import java.time.Duration
import org.apache.kafka.clients.consumer.{ConsumerRecord, KafkaConsumer}
import org.apache.kafka.common.serialization.StringDeserializer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.jdk.CollectionConverters._
import scala.util.Using

/**
 * Integration test of IOKafka: it sends rows to a Kafka broker (started in this JVM), and reads them back.
 */
class IOKafkaBrokerSpec extends AnyFlatSpec with should.Matchers:

  import IOKafkaBrokerSpec._

  behavior of "IOKafka.toKafka (with a broker)"

  it should "send rows which a consumer can read back, with the column names" in:
    EmbeddedKafka.withRunningKafkaOnFoundPort(EmbeddedKafkaConfig(kafkaPort = 0, controllerPort = 0)) { config =>
      val servers = s"localhost:${config.kafkaPort}"
      val settings = ProducerSettings[IO, String, String].withBootstrapServers(servers)
      Stream.emits(points).through(IOKafka.toKafka("points", settings)(p => s"${p.x}")).compile.drain.unsafeRunSync()
      val received = consume(servers, "points", points.size)
      received.map(r => (r.key(), r.value())) shouldBe List(("1", "1,2"), ("3", "3,4"), ("5", "5,6"))
      received.map(r => new String(r.headers().lastHeader(IOKafka.columnsHeader).value(), StandardCharsets.UTF_8)) shouldBe List("x,y", "x,y", "x,y")
    }

object IOKafkaBrokerSpec:
  case class Point(x: Int, y: Int) derives CsvProduct

  val points: List[Point] = List(Point(1, 2), Point(3, 4), Point(5, 6))

  // Read n records from the start of the topic (or as many as arrive within ten seconds).
  def consume(servers: String, topic: String, n: Int): List[ConsumerRecord[String, String]] =
    val properties: Map[String, AnyRef] = Map("bootstrap.servers" -> servers, "group.id" -> "IOKafkaBrokerSpec", "auto.offset.reset" -> "earliest")
    Using.resource(new KafkaConsumer[String, String](properties.asJava, new StringDeserializer, new StringDeserializer)) { consumer =>
      consumer.subscribe(List(topic).asJava)
      val deadline = System.currentTimeMillis() + 10000
      LazyList.continually(consumer.poll(Duration.ofMillis(500)).asScala.toList)
        .scanLeft(List.empty[ConsumerRecord[String, String]])(_ ++ _)
        .find(rs => rs.size >= n || System.currentTimeMillis() > deadline)
        .getOrElse(Nil)
    }
