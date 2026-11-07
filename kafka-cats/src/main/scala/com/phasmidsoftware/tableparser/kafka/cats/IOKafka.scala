package com.phasmidsoftware.tableparser.kafka.cats

import cats.effect.IO
import com.phasmidsoftware.tableparser.core.parse.StringTableParser
import com.phasmidsoftware.tableparser.core.render.{CsvGenerator, CsvRenderer}
import com.phasmidsoftware.tableparser.core.table.Table
import fs2.kafka.consumer.MkConsumer
import fs2.kafka.producer.MkProducer
import fs2.kafka.{CommittableOffset, CommittableOffsetBatch, ConsumerRecord, ConsumerSettings, Header, Headers, KafkaConsumer, KafkaProducer, ProducerRecord, ProducerResult, ProducerSettings}
import fs2.{Pipe, Stream}
import scala.util.Try

/**
 * Pipes which send rows to a Kafka topic, using fs2-kafka: one message for each row;
 * and streams which read them back (fromKafka, fromKafkaCommittable and fromKafkaWith).
 *
 * By default, the value of each message is the row rendered as one line of CSV (with no line ending),
 * and the column names, also as one line of CSV, are in the message's record header called columnsHeader.
 * So each message describes itself: a consumer may start at any offset, and needs nothing else to read it.
 * Alternatively, toKafkaWith takes a function to render each row (for example, as JSON), and adds no header.
 *
 * The rows can come from any stream, such as one made by IOTable.streamFile, or from a Table (Stream.emits(table.content.toSeq)).
 * For messages without a key, use Unit as the key type (with the key function _ => ()):
 * fs2-kafka serializes Unit as null, so Kafka chooses the partition.
 * The producer settings (such as batching, compression and acknowledgements) are fs2-kafka's.
 */
object IOKafka:

  /**
   * The name of the record header which carries the column names of a CSV message.
   */
  val columnsHeader: String = "tableparser.columns"

  /**
   * A Pipe which sends rows, as CSV, to a Kafka topic,
   * with a producer which is made when the stream starts and closed when it ends.
   *
   * @param topic     the topic.
   * @param settings  the fs2-kafka ProducerSettings (which include the key serializer).
   * @param key       a function to get the key of a row's message.
   * @param renderer  implicit CsvRenderer[X].
   * @param generator implicit CsvGenerator[X] (for the column names).
   * @param mk        implicit MkProducer[IO] (fs2-kafka's default makes a Kafka producer from the settings).
   * @tparam K the key type.
   * @tparam X the row type.
   * @return a Pipe[IO, X, ProducerResult[K, String]], which emits the result of each batch, once Kafka has acknowledged it.
   */
  def toKafka[K, X](topic: String, settings: ProducerSettings[IO, K, String])(key: X => K)(using renderer: CsvRenderer[X], generator: CsvGenerator[X], mk: MkProducer[IO]): Pipe[IO, X, ProducerResult[K, String]] =
    _.through(csvRecords(topic)(key)).through(produce(settings))

  /**
   * A Pipe which sends rows, each rendered by the given function, to a Kafka topic,
   * with a producer which is made when the stream starts and closed when it ends.
   * The messages have no columnsHeader.
   *
   * @param topic    the topic.
   * @param settings the fs2-kafka ProducerSettings (which include the key serializer).
   * @param key      a function to get the key of a row's message.
   * @param value    a function to render a row as the value of its message (for example, with circe, _.asJson.noSpaces).
   * @param mk       implicit MkProducer[IO] (fs2-kafka's default makes a Kafka producer from the settings).
   * @tparam K the key type.
   * @tparam X the row type.
   * @return a Pipe[IO, X, ProducerResult[K, String]], which emits the result of each batch, once Kafka has acknowledged it.
   */
  def toKafkaWith[K, X](topic: String, settings: ProducerSettings[IO, K, String])(key: X => K, value: X => String)(using mk: MkProducer[IO]): Pipe[IO, X, ProducerResult[K, String]] =
    _.through(records(topic)(key, value)).through(produce(settings))

  /**
   * A Pipe which makes the Kafka record of each row: its value is the row as CSV, and its columnsHeader the column names.
   *
   * @param topic     the topic.
   * @param key       a function to get the key of a row's message.
   * @param renderer  implicit CsvRenderer[X].
   * @param generator implicit CsvGenerator[X] (for the column names).
   * @tparam K the key type.
   * @tparam X the row type.
   * @return a Pipe[IO, X, ProducerRecord[K, String]].
   */
  def csvRecords[K, X](topic: String)(key: X => K)(using renderer: CsvRenderer[X], generator: CsvGenerator[X]): Pipe[IO, X, ProducerRecord[K, String]] =
    val headers = Headers(Header(columnsHeader, CsvGenerator.header[X]))
    _.map(x => ProducerRecord(topic, key(x), renderer.render(x, Map())).withHeaders(headers))

  /**
   * A Pipe which makes the Kafka record of each row, with its value rendered by the given function (and no headers).
   *
   * @param topic the topic.
   * @param key   a function to get the key of a row's message.
   * @param value a function to render a row as the value of its message.
   * @tparam K the key type.
   * @tparam X the row type.
   * @return a Pipe[IO, X, ProducerRecord[K, String]].
   */
  def records[K, X](topic: String)(key: X => K, value: X => String): Pipe[IO, X, ProducerRecord[K, String]] =
    _.map(x => ProducerRecord(topic, key(x), value(x)))

  /**
   * A Pipe which sends records to Kafka, a chunk at a time,
   * with a producer which is made when the stream starts and closed when it ends.
   *
   * @param settings the fs2-kafka ProducerSettings.
   * @param mk       implicit MkProducer[IO].
   * @tparam K the key type.
   * @return a Pipe[IO, ProducerRecord[K, String], ProducerResult[K, String]].
   */
  def produce[K](settings: ProducerSettings[IO, K, String])(using mk: MkProducer[IO]): Pipe[IO, ProducerRecord[K, String], ProducerResult[K, String]] =
    _.chunks.through(KafkaProducer.pipe(settings))

  // ── Reading rows from Kafka (#113) ─────────────────────────────────────────────

  /**
   * Method to read the rows of a topic, as a stream, with a consumer which is made when the stream starts
   * and closed when it ends: the mirror of toKafka (and of IOTable.streamFile).
   * Each message's value is parsed as one row, with the column names in its columnsHeader record header
   * (or the parser's fixed header, if a message has none): see parseRecord.
   * The parser's raw predicate, predicate and forgiving mode apply, as they do to the rows of a file.
   *
   * The offsets are committed a chunk at a time (as Kafka delivered them), once the chunk's rows have been emitted:
   * so if the stream fails, a row may be read again when it restarts (at-least-once). To commit only after your own
   * processing of each row, use fromKafkaCommittable instead. The keys of the messages are ignored.
   *
   * The parser is passed as a value (not a given), so that the type of the rows is known.
   *
   * @param topic    the topic.
   * @param settings the fs2-kafka ConsumerSettings (with the group id, auto.offset.reset and so on).
   * @param parser   a StringTableParser (e.g. a HeadedCSVTableParser).
   * @param mk       implicit MkConsumer[IO] (fs2-kafka's default makes a Kafka consumer from the settings).
   * @tparam K the key type.
   * @return a Stream[IO, parser.Row].
   */
  def fromKafka[K](topic: String, settings: ConsumerSettings[IO, K, String])(parser: StringTableParser[?])(using mk: MkConsumer[IO]): Stream[IO, parser.Row] =
    committing(fromKafkaCommittable(topic, settings)(parser))

  /**
   * Method to read the rows of a topic, each with the offset of its message, which the caller commits
   * (for example, with fs2-kafka's commitBatchWithin) once it has processed the row. See fromKafka.
   * A message whose row is left out (by a predicate, or because the parser is forgiving) emits nothing,
   * so its offset is committed only with that of a later row.
   *
   * @param topic    the topic.
   * @param settings the fs2-kafka ConsumerSettings.
   * @param parser   a StringTableParser.
   * @param mk       implicit MkConsumer[IO].
   * @tparam K the key type.
   * @return a Stream[IO, (parser.Row, CommittableOffset[IO])].
   */
  def fromKafkaCommittable[K](topic: String, settings: ConsumerSettings[IO, K, String])(parser: StringTableParser[?])(using mk: MkConsumer[IO]): Stream[IO, (parser.Row, CommittableOffset[IO])] =
    KafkaConsumer.stream(settings).subscribeTo(topic).records.evalMapFilter: r =>
      IO.fromTry(parseRecord(parser)(r.record)).map(_.map(_ -> r.offset))

  /**
   * Method to read the messages of a topic, each decoded by the given function (for example, JSON with circe's decode):
   * the mirror of toKafkaWith. The offsets are committed as by fromKafka.
   *
   * @param topic    the topic.
   * @param settings the fs2-kafka ConsumerSettings.
   * @param decode   a function to decode a message's value (a Failure fails the stream).
   * @param mk       implicit MkConsumer[IO].
   * @tparam K the key type.
   * @tparam X the type of the decoded values.
   * @return a Stream[IO, X].
   */
  def fromKafkaWith[K, X](topic: String, settings: ConsumerSettings[IO, K, String])(decode: String => Try[X])(using mk: MkConsumer[IO]): Stream[IO, X] =
    committing(KafkaConsumer.stream(settings).subscribeTo(topic).records.evalMap(r => IO.fromTry(decode(r.record.value)).map(_ -> r.offset)))

  /**
   * Method to parse the value of a consumer record as a row, with the column names in its columnsHeader (see Table.parseRow).
   *
   * @param parser a StringTableParser.
   * @param record the ConsumerRecord.
   * @tparam K the key type.
   * @return a Try of the row: Success(None) if it is left out (by a predicate, or by a forgiving parser).
   */
  def parseRecord[K](parser: StringTableParser[?])(record: ConsumerRecord[K, String]): Try[Option[parser.Row]] =
    Table.parseRow(parser)(record.headers.withKey(columnsHeader).map(_.as[String]), record.value)

  // Emit the values of each chunk, then commit the chunk's offsets.
  private def committing[X](s: Stream[IO, (X, CommittableOffset[IO])]): Stream[IO, X] =
    s.chunks.flatMap: chunk =>
      Stream.chunk(chunk.map(_._1)) ++ Stream.exec(CommittableOffsetBatch.fromFoldable(chunk.map(_._2).toList).commit)
