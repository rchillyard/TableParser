package com.phasmidsoftware.parse

import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.util.Failure

class ImplicitParserSpec extends flatspec.AnyFlatSpec with should.Matchers {

  import com.phasmidsoftware.table.MovieParser.MovieTableParser
  import com.phasmidsoftware.table.{Movie, Table}

  import scala.io.Source
  import scala.util.{Success, Try}

  behavior of "implicit class"

  private val source = Source.fromURL(classOf[Table[_]].getResource("movie_metadata.csv"))

  private val parser = MovieTableParser

  private val ty = parser parse source

  ty match {
    case Success(t) => println(s"Table read with ${t.rows} rows")
    case Failure(x) => fail(x)
  }


}
