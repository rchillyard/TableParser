package com.phasmidsoftware.tableparser.kafka.cats

import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.tableparser.cats.table.IOTable
import com.phasmidsoftware.tableparser.kafka.cats.TripsToKafka.{TripTableParser, tripsFile}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

/**
 * Tests of the file of KAFKA.md's example (no broker is needed).
 */
class TripsToKafkaSpec extends AnyFlatSpec with should.Matchers:

  behavior of "TripsToKafka"

  it should "find yellowtaxitrip.csv, and parse each of its thousand trips" in:
    IOTable.streamFile(TripTableParser)(tripsFile).compile.count.unsafeRunSync() shouldBe 1000
