package com.phasmidsoftware.parse

import org.scalatest.flatspec
import org.scalatest.matchers.should
import scala.io.BufferedSource
import scala.util.Failure

class ImplicitParserSpec extends flatspec.AnyFlatSpec with should.Matchers {

  import com.phasmidsoftware.table.MovieParser.MovieTableParser
  import com.phasmidsoftware.table.Table
  import scala.io.Source
  import scala.util.Success

  behavior of "implicit class"

  it should "properly parse movie data" in {
    val source = Source.fromURL(classOf[Table[_]].getResource("movie_metadata.csv"))
    val parser = MovieTableParser
    val ty = parser parse source

    ty match {
      case Success(t) => println(s"Table read with ${t.rows} rows")
      case Failure(x) => fail(x)
    }
  }

  class MySource(s: Source) extends Source {
    protected val iter: Iterator[Char] = s match {
      case b: BufferedSource => b.iter
      case _ => fail("wrong type of Source")
    }
    var open: Boolean = true

    override def close(): Unit = {
      super.close()
      open = false
    }
  }

  // TODO use a smaller data set to save time.
  it should "properly close the source" in {
    val source = new MySource(Source.fromURL(classOf[Table[_]].getResource("movie_metadata.csv")))
    source.open shouldBe true
    val parser = MovieTableParser
    val ty = parser parse source
    ty match {
      case Success(_) => source.open shouldBe false
      case Failure(x) => fail(x)
    }
  }

}
