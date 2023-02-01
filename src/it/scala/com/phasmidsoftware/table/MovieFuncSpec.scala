package com.phasmidsoftware.table

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.parse.TableParser
import com.phasmidsoftware.util.TryUsing
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source
import scala.util.{Success, Try}

class MovieFuncSpec extends AnyFlatSpec with Matchers {

  behavior of "Movie table"

  /**
   * NOTE: it is perfectly proper for there to be a number of parsing problems.
   * These are application-specific and are not indicative of any bugs in the
   * TableParser library itself.
   */
  it should "be ingested properly" in {
    import MovieParser._

    val mty: IO[Table[Movie]] = Table.parseResource("movie_metadata.csv")
    val mt: Table[Movie] = mty.unsafeRunSync()
    mt should matchPattern { case HeadedTable(_, _) => }
    println(s"Movie: successfully read ${mt.size} rows")
    mt.size shouldBe 1567
    mt take 10 foreach println
  }

  /**
   * NOTE: it is perfectly proper for there to be a number of parsing problems.
   * These are application-specific and are not indicative of any bugs in the
   * TableParser library itself.
   *
   * FIXME
   */
    ignore should "be ingested and written out properly" in {
      import MovieParser._

//      implicit val parser: TableParser[Table[Movie]] = implicitly[TableParser[Table[Movie]]]
//      val mty: Try[Table[Movie]] = TryUsing(Source.fromURL(classOf[Movie].getResource("movie_metadata.csv")))(parse(_))
//      mty should matchPattern { case Success(HeadedTable(_, _)) => }
//      for (mt <- mty) {
//        // TODO implement this part
//        //      mt.generateCsvHeader
//        //      mt.toCSV
//      }
    }

  // FIXME
    it should "parse and filter the movies from the IMDB dataset" in {
      import MovieParser._
      implicit val parser: TableParser[Table[Movie]] = implicitly[TableParser[Table[Movie]]]
      implicit val hasKey: HasKey[Movie] = (t: Movie) => t.production.country
//      val mty: Try[Table[Movie]] = TryUsing(Source.fromURL(classOf[Movie].getResource("movie_metadata.csv")))(parse(_))
//      mty should matchPattern { case Success(HeadedTable(_, _)) => }
//      val mt = mty.get
//      val kiwiMovies = mt.filterByKey(_ == "New Zealand")
//      kiwiMovies.size shouldBe 4
    }

}
