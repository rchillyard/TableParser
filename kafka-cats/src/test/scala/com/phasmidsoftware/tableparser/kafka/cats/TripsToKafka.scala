package com.phasmidsoftware.tableparser.kafka.cats

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.tableparser.cats.table.IOTable
import com.phasmidsoftware.tableparser.core.parse.ColumnHelper.camelToSnakeCaseColumnNameMapperLower
import com.phasmidsoftware.tableparser.core.parse.{CellParser, CellParsers, ColumnHelper, HeadedCSVTableParser, RowConfig, StandardRowParser, StringParser}
import com.phasmidsoftware.tableparser.core.render.{CsvProduct, SparkSchema}
import fs2.kafka.{AutoOffsetReset, ConsumerSettings, ProducerSettings}
import java.nio.file.{Path, Paths}
import java.time.Instant

/**
 * The runnable example of KAFKA.md: it streams the taxi trips of a CSV file to a Kafka topic, one CSV message for each row.
 * The rows are parsed as the file is read, and sent a chunk at a time, so the file can be of any size.
 *
 * With a broker running (see docker-compose.yml), from the root of the repository:
 * {{{
 *   sbt "kafkaCats/Test/runMain com.phasmidsoftware.tableparser.kafka.cats.sendTrips"
 * }}}
 * The optional arguments are the CSV file (yellowtaxitrip.csv), the bootstrap servers (localhost:9092) and the topic (trips).
 * Then `sbt "kafkaCats/Test/runMain com.phasmidsoftware.tableparser.kafka.cats.readTrips"` reads the first five trips back.
 */
object TripsToKafka:

  /**
   * A few of the columns of a yellow taxi trip: the other columns of the file are ignored.
   * SparkSchema gives the schema with which Spark's from_csv reads the messages (see SparkSchema.ddl).
   */
  case class Trip(vendorId: Int, pickup: Instant, dropoff: Instant, passengerCount: Int, tripDistance: Double, fareAmount: Double, tipAmount: Double, totalAmount: Double) derives CsvProduct, SparkSchema

  object Trip extends CellParsers:
    // The file has a space after each comma.
    given RowConfig = RowConfig.trimmingRowConfig
    given ColumnHelper[Trip] = columnHelper(
      camelToSnakeCaseColumnNameMapperLower
      , "vendorId" -> "VendorID"
      , "pickup" -> "tpep_pickup_datetime"
      , "dropoff" -> "tpep_dropoff_datetime"
    )
    given CellParser[Trip] = CellParser.derived[Trip]
    given StringParser[Trip] = StandardRowParser.create[Trip]

  object TripTableParser extends HeadedCSVTableParser[Trip]

  /**
   * Stream the trips of a file to a topic, each keyed by its vendor.
   *
   * @param path     the CSV file.
   * @param servers  the Kafka bootstrap servers.
   * @param topic    the topic.
   * @return an IO of the number of messages which Kafka has acknowledged.
   */
  def send(path: Path, servers: String, topic: String): IO[Int] =
    val settings = ProducerSettings[IO, String, String].withBootstrapServers(servers)
    IOTable.streamFile(TripTableParser)(path)
      .through(IOKafka.toKafka(topic, settings)(_.vendorId.toString))
      .map(_.size)
      .compile.foldMonoid

  /**
   * Read the first trips of a topic back, as rows (see IOKafka.fromKafka).
   *
   * @param n       the number of trips.
   * @param servers the Kafka bootstrap servers.
   * @param topic   the topic.
   * @param group   the consumer group.
   * @return an IO of the trips.
   */
  def read(n: Int, servers: String, topic: String, group: String): IO[List[Trip]] =
    val settings = ConsumerSettings[IO, String, String].withBootstrapServers(servers).withGroupId(group).withAutoOffsetReset(AutoOffsetReset.Earliest)
    IOKafka.fromKafka(topic, settings)(TripTableParser).take(n).compile.toList

/**
 * Send the trips of a CSV file to Kafka: see TripsToKafka.
 *
 * @param args the CSV file, the bootstrap servers and the topic (all optional).
 */
@main def sendTrips(args: String*): Unit =
  val path = args.headOption.getOrElse("yellowtaxitrip.csv")
  val servers = args.lift(1).getOrElse("localhost:9092")
  val topic = args.lift(2).getOrElse("trips")
  val n = TripsToKafka.send(Paths.get(path), servers, topic).unsafeRunSync()
  println(s"Sent $n messages from $path to topic $topic at $servers")
  // The schema with which Spark's from_csv reads them (see KAFKA.md).
  SparkSchema.ddl[TripsToKafka.Trip].foreach(schema => println(s"Spark schema: $schema"))

/**
 * Read the first trips of a topic back, and print them: see TripsToKafka.read.
 *
 * @param args the number of trips (5), the bootstrap servers (localhost:9092), the topic (trips) and the consumer group (readTrips).
 */
@main def readTrips(args: String*): Unit =
  val n = args.headOption.map(_.toInt).getOrElse(5)
  val servers = args.lift(1).getOrElse("localhost:9092")
  val topic = args.lift(2).getOrElse("trips")
  val group = args.lift(3).getOrElse("readTrips")
  TripsToKafka.read(n, servers, topic, group).unsafeRunSync().foreach(println)
