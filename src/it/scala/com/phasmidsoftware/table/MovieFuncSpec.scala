package com.phasmidsoftware.table

import org.scalatest.{FlatSpec, Matchers}

import scala.util._

class MovieFuncSpec extends FlatSpec with Matchers {

  behavior of "Movie table"

  /**
    * NOTE: it is perfectly proper for there to be a number of parsing problems.
    * These are application-specific and are not indicative of any bugs in the
    * TableParser library itself.
    */
  it should "be ingested properly" in {
    import MovieParser._

    val x: Try[Table[Movie]] = Table.parseResource("movie_metadata.csv")
    x should matchPattern { case Success(TableWithHeader(_, _)) => }
    val mt = x.get
    println(s"Movie: successfully read ${mt.size} rows")
    mt.size shouldBe 1567
    mt take 10 foreach println
  }

}
