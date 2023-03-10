package com.phasmidsoftware.parse

import cats.effect.IO
import com.phasmidsoftware.table.MovieParser.MovieTableParser
import com.phasmidsoftware.table.{HeadedTable, Movie, Table}
import com.phasmidsoftware.util.CheckIO.checkResultIO
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec
import org.scalatest.matchers.should
import org.scalatest.time.{Seconds, Span}
import scala.io.BufferedSource

class ImplicitParserSpec extends flatspec.AnyFlatSpec with should.Matchers {

  import scala.io.Source

  behavior of "implicit class"

  it should "properly parse movie data" in {
    val si = IO(Source.fromURL(classOf[Table[_]].getResource("movie_metadata.csv")))
    val parser = MovieTableParser
    // NOTE we should not directly invoke parse: rather, we should invoke parse(IO[...])
    val parsed: IO[Table[Movie]] = parser.parse(si)
    checkResultIO(parsed, Timeout(Span(5, Seconds))) {
      case tm@HeadedTable(_, _) => println(s"Table read with ${tm.rows} rows")
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
  //  it should "properly close the source" in {
  //    val source = new MySource(Source.fromURL(classOf[Table[_]].getResource("movie_metadata.csv")))
  //    source.open shouldBe true
  //    val parser = MovieTableParser
  //    val ty = parser parse source
  //    ty match {
  //      case Success(_) => source.open shouldBe false
  //      case Failure(x) => fail(x)
  //    }
  //  }

}
