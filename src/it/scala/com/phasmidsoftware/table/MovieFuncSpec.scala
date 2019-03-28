package com.phasmidsoftware.table

import org.scalatest.{FlatSpec, Matchers}

import scala.util._

class MovieFuncSpec extends FlatSpec with Matchers {

  behavior of "Movie table"

  it should "be ingested properly" in {
    import MovieFormat._

    val x: Try[Table[Movie]] = Table.parseResource("movie_metadata.csv")
    x should matchPattern { case Success(TableWithoutHeader(_)) => }
    val mt = x.get
    println(s"Movie: successfully read ${mt.size} rows")
    mt.size shouldBe 1488
    mt take 10 foreach println
  }

}
