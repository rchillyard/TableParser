package com.phasmidsoftware.tableparser.kafka.zio

import com.phasmidsoftware.tableparser.core.parse.StringTableParser
import com.phasmidsoftware.tableparser.core.render.{CsvGenerator, CsvRenderer}
import com.phasmidsoftware.tableparser.core.table.Table
import java.nio.charset.StandardCharsets
import org.apache.kafka.clients.consumer.ConsumerRecord
import org.apache.kafka.clients.producer.{ProducerRecord, RecordMetadata}
import org.apache.kafka.common.header.Header
import org.apache.kafka.common.header.internals.RecordHeader
import scala.jdk.CollectionConverters._
import scala.util.Try
import zio.ZIO
import zio.kafka.consumer.{CommittableRecord, Consumer, Offset, OffsetBatch, Subscription}
import zio.kafka.producer.Producer
import zio.kafka.serde.{Deserializer, Serde, Serializer}
import zio.stream.{ZPipeline, ZStream}

/**
 * ZPipelines which send rows to a Kafka topic, using zio-kafka: one message for each row;
 * and streams which read them back (fromKafka, fromKafkaCommittable and fromKafkaWith).
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

  // ── Reading rows from Kafka (#113) ─────────────────────────────────────────────

  /**
   * Method to read the rows of a topic, as a stream which needs a zio-kafka Consumer (for example, from
   * Consumer.make(ConsumerSettings(servers).withGroupId(group))): the mirror of toKafka (and of ZIOTable.streamFile).
   * Each message's value is parsed as one row, with the column names in its columnsHeader record header
   * (or the parser's fixed header, if a message has none): see parseRecord.
   * The parser's raw predicate, predicate and forgiving mode apply, as they do to the rows of a file.
   *
   * The offsets are committed a chunk at a time (as Kafka delivered them), once the chunk's rows have been emitted:
   * so if the stream fails, a row may be read again when it restarts (at-least-once). To commit only after your own
   * processing of each row, use fromKafkaCommittable instead.
   *
   * @param topic           the topic.
   * @param keyDeserializer the Deserializer of the keys (for example, Serde.string); the keys are ignored.
   * @param parser          a StringTableParser (e.g. a HeadedCSVTableParser).
   * @tparam K the key type.
   * @return a ZStream[Consumer, Throwable, parser.Row].
   */
  def fromKafka[K](topic: String, keyDeserializer: Deserializer[Any, K])(parser: StringTableParser[?]): ZStream[Consumer, Throwable, parser.Row] =
    committing(fromKafkaCommittable(topic, keyDeserializer)(parser))

  /**
   * Method to read the rows of a topic, each with the Offset of its message, which the caller commits
   * (for example, by aggregating them with Consumer.offsetBatches) once it has processed the row. See fromKafka.
   * A message whose row is left out (by a predicate, or because the parser is forgiving) emits nothing,
   * so its offset is committed only with that of a later row.
   *
   * @param topic           the topic.
   * @param keyDeserializer the Deserializer of the keys.
   * @param parser          a StringTableParser.
   * @tparam K the key type.
   * @return a ZStream[Consumer, Throwable, (parser.Row, Offset)].
   */
  def fromKafkaCommittable[K](topic: String, keyDeserializer: Deserializer[Any, K])(parser: StringTableParser[?]): ZStream[Consumer, Throwable, (parser.Row, Offset)] =
    plain(topic, keyDeserializer)
      .mapZIO(r => ZIO.fromTry(parseRecord(parser)(r.record)).map(_.map(_ -> r.offset)))
      .collectSome

  /**
   * Method to read the messages of a topic, each decoded by the given function (for example, JSON with circe's decode):
   * the mirror of toKafkaWith. The offsets are committed as by fromKafka.
   *
   * @param topic           the topic.
   * @param keyDeserializer the Deserializer of the keys.
   * @param decode          a function to decode a message's value (a Failure fails the stream).
   * @tparam K the key type.
   * @tparam X the type of the decoded values.
   * @return a ZStream[Consumer, Throwable, X].
   */
  def fromKafkaWith[K, X](topic: String, keyDeserializer: Deserializer[Any, K])(decode: String => Try[X]): ZStream[Consumer, Throwable, X] =
    committing(plain(topic, keyDeserializer).mapZIO(r => ZIO.fromTry(decode(r.value)).map(_ -> r.offset)))

  /**
   * Method to parse the value of a consumer record as a row, with the column names in its columnsHeader (see Table.parseRow).
   *
   * @param parser a StringTableParser.
   * @param record the Kafka ConsumerRecord.
   * @tparam K the key type.
   * @return a Try of the row: Success(None) if it is left out (by a predicate, or by a forgiving parser).
   */
  def parseRecord[K](parser: StringTableParser[?])(record: ConsumerRecord[K, String]): Try[Option[parser.Row]] =
    val maybeHeader = Option(record.headers().lastHeader(columnsHeader)).map(h => new String(h.value(), StandardCharsets.UTF_8))
    Table.parseRow(parser)(maybeHeader, record.value())

  // Emit the values of each chunk, then commit the chunk's offsets.
  private def committing[R, X](s: ZStream[R, Throwable, (X, Offset)]): ZStream[R, Throwable, X] =
    s.chunks.flatMap: chunk =>
      ZStream.fromChunk(chunk.map(_._1)) ++ ZStream.fromZIO(chunk.foldLeft(OffsetBatch.empty)((b, p) => b.add(p._2)).commit).drain

  // The records of a topic, from the Consumer in the environment.
  private def plain[K](topic: String, keyDeserializer: Deserializer[Any, K]): ZStream[Consumer, Throwable, CommittableRecord[K, String]] =
    ZStream.serviceWithStream[Consumer](_.plainStream(Subscription.topics(topic), keyDeserializer, Serde.string))
