package com.phasmidsoftware.tableparser.kafka.cats

import cats.effect.IO
import com.phasmidsoftware.tableparser.core.render.{CsvGenerator, CsvRenderer}
import fs2.Pipe
import fs2.kafka.producer.MkProducer
import fs2.kafka.{Header, Headers, KafkaProducer, ProducerRecord, ProducerResult, ProducerSettings}

/**
 * Pipes which send rows to a Kafka topic, using fs2-kafka: one message for each row.
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
