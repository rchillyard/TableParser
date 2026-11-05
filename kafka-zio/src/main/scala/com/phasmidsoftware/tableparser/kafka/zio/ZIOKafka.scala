package com.phasmidsoftware.tableparser.kafka.zio

import com.phasmidsoftware.tableparser.core.render.{CsvGenerator, CsvRenderer}
import java.nio.charset.StandardCharsets
import org.apache.kafka.clients.producer.{ProducerRecord, RecordMetadata}
import org.apache.kafka.common.header.Header
import org.apache.kafka.common.header.internals.RecordHeader
import scala.jdk.CollectionConverters._
import zio.ZIO
import zio.kafka.producer.Producer
import zio.kafka.serde.{Serde, Serializer}
import zio.stream.ZPipeline

/**
 * ZPipelines which send rows to a Kafka topic, using zio-kafka: one message for each row.
 *
 * By default, the value of each message is the row rendered as one line of CSV (with no line ending),
 * and the column names, also as one line of CSV, are in the message's record header called columnsHeader.
 * So each message describes itself: a consumer may start at any offset, and needs nothing else to read it.
 * Alternatively, toKafkaWith takes a function to render each row (for example, as JSON), and adds no header.
 *
 * The rows can come from any stream, such as one made by ZIOTable.streamFile, or from a Table (ZStream.fromIterable(table.content.toSeq)).
 * The pipelines need a zio-kafka Producer (for example, from Producer.make(ProducerSettings(servers))),
 * whose settings (such as batching, compression and acknowledgements) are zio-kafka's.
 * For messages without a key, use unitSerializer, with the key function _ => (): Kafka then chooses the partition.
 */
object ZIOKafka:

  /**
   * The name of the record header which carries the column names of a CSV message.
   */
  val columnsHeader: String = "tableparser.columns"

  /**
   * A key Serializer for messages without a key: it serializes () as null.
   */
  val unitSerializer: Serializer[Any, Unit] = Serializer((_, _, _) => ZIO.succeed(null: Array[Byte]))

  /**
   * A ZPipeline which sends rows, as CSV, to a Kafka topic.
   *
   * @param topic         the topic.
   * @param keySerializer the zio-kafka Serializer of the keys (for example, Serde.string, or unitSerializer).
   * @param key           a function to get the key of a row's message.
   * @param renderer      implicit CsvRenderer[X].
   * @param generator     implicit CsvGenerator[X] (for the column names).
   * @tparam K the key type.
   * @tparam X the row type.
   * @return a ZPipeline[Producer, Throwable, X, RecordMetadata], which emits each message's metadata, once Kafka has acknowledged it.
   */
  def toKafka[K, X](topic: String, keySerializer: Serializer[Any, K])(key: X => K)(using renderer: CsvRenderer[X], generator: CsvGenerator[X]): ZPipeline[Producer, Throwable, X, RecordMetadata] =
    csvRecords(topic)(key) >>> produce(keySerializer)

  /**
   * A ZPipeline which sends rows, each rendered by the given function, to a Kafka topic.
   * The messages have no columnsHeader.
   *
   * @param topic         the topic.
   * @param keySerializer the zio-kafka Serializer of the keys (for example, Serde.string, or unitSerializer).
   * @param key           a function to get the key of a row's message.
   * @param value         a function to render a row as the value of its message (for example, with circe, _.asJson.noSpaces).
   * @tparam K the key type.
   * @tparam X the row type.
   * @return a ZPipeline[Producer, Throwable, X, RecordMetadata], which emits each message's metadata, once Kafka has acknowledged it.
   */
  def toKafkaWith[K, X](topic: String, keySerializer: Serializer[Any, K])(key: X => K, value: X => String): ZPipeline[Producer, Throwable, X, RecordMetadata] =
    records(topic)(key, value) >>> produce(keySerializer)

  /**
   * A ZPipeline which makes the Kafka record of each row: its value is the row as CSV, and its columnsHeader the column names.
   *
   * @param topic     the topic.
   * @param key       a function to get the key of a row's message.
   * @param renderer  implicit CsvRenderer[X].
   * @param generator implicit CsvGenerator[X] (for the column names).
   * @tparam K the key type.
   * @tparam X the row type.
   * @return a ZPipeline[Any, Nothing, X, ProducerRecord[K, String]].
   */
  def csvRecords[K, X](topic: String)(key: X => K)(using renderer: CsvRenderer[X], generator: CsvGenerator[X]): ZPipeline[Any, Nothing, X, ProducerRecord[K, String]] =
    val columns = CsvGenerator.header[X].getBytes(StandardCharsets.UTF_8)
    // NOTE: each record gets its own header, since a Kafka record's headers are mutable.
    ZPipeline.map(x => record(topic, key(x), renderer.render(x, Map()), List(new RecordHeader(columnsHeader, columns))))

  /**
   * A ZPipeline which makes the Kafka record of each row, with its value rendered by the given function (and no headers).
   *
   * @param topic the topic.
   * @param key   a function to get the key of a row's message.
   * @param value a function to render a row as the value of its message.
   * @tparam K the key type.
   * @tparam X the row type.
   * @return a ZPipeline[Any, Nothing, X, ProducerRecord[K, String]].
   */
  def records[K, X](topic: String)(key: X => K, value: X => String): ZPipeline[Any, Nothing, X, ProducerRecord[K, String]] =
    ZPipeline.map(x => record(topic, key(x), value(x), Nil))

  /**
   * A ZPipeline which sends records to Kafka, a chunk at a time, with the Producer from the environment.
   * Each chunk is acknowledged before the next is sent.
   *
   * @param keySerializer the zio-kafka Serializer of the keys.
   * @tparam K the key type.
   * @return a ZPipeline[Producer, Throwable, ProducerRecord[K, String], RecordMetadata].
   */
  def produce[K](keySerializer: Serializer[Any, K]): ZPipeline[Producer, Throwable, ProducerRecord[K, String], RecordMetadata] =
    ZPipeline.mapChunksZIO(records => ZIO.serviceWithZIO[Producer](_.produceChunk(records, keySerializer, Serde.string)))

  // NOTE: the partition is null, so that Kafka chooses it.
  private def record[K](topic: String, key: K, value: String, headers: List[Header]): ProducerRecord[K, String] =
    new ProducerRecord[K, String](topic, null, key, value, headers.asJava)
