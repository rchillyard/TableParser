package com.phasmidsoftware.table

import com.phasmidsoftware.parse._
import org.scalatest.{FlatSpec, Matchers}

import scala.util._

class MovieFuncSpec extends FlatSpec with Matchers {

  behavior of "Movie table"

  // TODO enable us to perform this test successfully
  ignore should "be ingested properly" in {
    import MovieFormat._

    println(implicitly[RowParser[Movie]])
    val x: Try[Table[Movie]] = for (r <- Table.parseResource("movie_metadata.csv", classOf[MovieFuncSpec])) yield r
    x should matchPattern { case Success(TableWithoutHeader(_)) => }

  }

}
