package com.phasmidsoftware.tableparser.kafka.cats

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.tableparser.cats.table.IOTable
import com.phasmidsoftware.tableparser.core.parse.ColumnHelper.camelToSnakeCaseColumnNameMapperLower
import com.phasmidsoftware.tableparser.core.parse.{CellParser, CellParsers, ColumnHelper, HeadedCSVTableParser, RowConfig, StandardRowParser, StringParser}
import com.phasmidsoftware.tableparser.core.render.CsvProduct
import fs2.kafka.ProducerSettings
import java.nio.file.{Path, Paths}

/**
 * The runnable example of KAFKA.md: it streams the taxi trips of a CSV file to a Kafka topic, one CSV message for each row.
 * The rows are parsed as the file is read, and sent a chunk at a time, so the file can be of any size.
 *
 * With a broker running (see docker-compose.yml), from the root of the repository:
 * {{{
 *   sbt "kafkaCats/Test/runMain com.phasmidsoftware.tableparser.kafka.cats.sendTrips"
 * }}}
 * The optional arguments are the CSV file (yellowtaxitrip.csv), the bootstrap servers (localhost:9092) and the topic (trips).
 */
object TripsToKafka:

  /**
   * A few of the columns of a yellow taxi trip: the other columns of the file are ignored.
   * The times are kept as Strings (as they are in the file): Spark reads them as timestamps.
   */
  case class Trip(vendorId: Int, pickup: String, dropoff: String, passengerCount: Int, tripDistance: Double, fareAmount: Double, tipAmount: Double, totalAmount: Double) derives CsvProduct

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
