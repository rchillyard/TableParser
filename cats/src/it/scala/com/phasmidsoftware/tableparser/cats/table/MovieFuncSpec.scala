package com.phasmidsoftware.tableparser.cats.table

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.tableparser.cats.util.EvaluateIO.matchIO
import com.phasmidsoftware.tableparser.cats.util.IOUsing
import com.phasmidsoftware.tableparser.core.render._
import com.phasmidsoftware.tableparser.core.table._
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Seconds, Span}
import scala.io.Source

class MovieFuncSpec extends AnyFlatSpec with Matchers {

  behavior of "Movie table"

  /**
   * NOTE: it is perfectly proper for there to be a number of parsing problems.
   * These are application-specific and are not indicative of any bugs in the
   * TableParser library itself.
   */
  it should "be ingested properly" in {
    import com.phasmidsoftware.tableparser.core.table.MovieParser._

    val mti: IO[Table[Movie]] = IO.fromTry(Table.parseResource("movie_metadata.csv"))
    matchIO(mti, Timeout(Span(4, Seconds))) {
      (mt: Table[Movie]) =>
        mt should matchPattern { case HeadedTable(_, _) => }
        println(s"Movie: successfully read ${mt.size} rows")
        mt.size shouldBe 1567
        mt take 10 foreach println
        succeed
    }
  }
  
  it should "not be ingested properly" in {
    import com.phasmidsoftware.tableparser.core.table.MovieParser._
    var errorString = ""
    val errorFunc: Throwable => IO[Unit] = e => IO{errorString = e.getLocalizedMessage}

    val mti: IO[Table[Movie]] = IO.fromTry(Table.parseResource("movie_metadataX.csv")).onError(errorFunc)
    // TODO eliminate use of unsafe
    mti.attempt.unsafeRunSync() should matchPattern { case Left(_) => }
    errorString shouldBe "Table.sourceFromClassResource: cannot find resource 'movie_metadataX.csv' relative to class com.phasmidsoftware.tableparser.core.table.Table$"
  }

  /**
   * NOTE: it is perfectly proper for there to be a number of parsing problems.
   * These are application-specific and are not indicative of any bugs in the
   * TableParser library itself.
   */
  it should "be ingested and written out properly" in {
    import com.phasmidsoftware.tableparser.core.table.MovieParser._

    val mti: IO[Table[Movie]] = IOUsing(Source.fromURL(classOf[Movie].getResource("movie_metadata.csv")))(x => IO.fromTry(Table.parseSource(x)))
    // TODO create a combined renderer/generator for Movie
    implicit val csvRenderer: CsvRenderer[Movie] = new CsvRendererMovie
    implicit val csvGenerator: CsvGenerator[Movie] = Movie.createMovieCvsGenerator

    val wi: IO[String] = mti flatMap (mt => IO.fromTry(mt.toCSV)) // for (mt <- mti) yield mt.toCSV
    // TODO eliminate use of unsafe methods
    wi.unsafeRunSync().startsWith(
      """title,format.color,format.language,format.aspectRatio,format.duration,production.country,production.budget,production.gross,production.titleYear,reviews.imdbScore,reviews.facebookLikes,reviews.contentRating.code,reviews.contentRating.age,reviews.numUsersReview,reviews.numUsersVoted,reviews.numCriticReviews,reviews.totalFacebookLikes,director.name.first,director.name.middle,director.name.last,director.name.suffix,director.facebookLikes,actor1.name.first,actor1.name.middle,actor1.name.last,actor1.name.suffix,actor1.facebookLikes,actor2.name.first,actor2.name.middle,actor2.name.last,actor2.name.suffix,actor2.facebookLikes,actor3,genres.xs,plotKeywords.xs,imdb
        |Avatar,Color,English,1.78,178,USA,237000000,760505847,2009,7.9,33000,PG,13,3054,886204,723,4834,James,,Cameron,,0,CCH,,Pounder,,1000,Joel,David,Moore,,936,Wes,,Studi,,855,Action,Adventure,Fantasy,Sci-Fi,avatar,future,marine,native,paraplegic,http://www.imdb.com/title/tt0499549/?ref_=fn_tt_tt_1""".stripMargin) shouldBe true
  }

  it should "parse and filter the movies from the IMDB dataset" in {
    import com.phasmidsoftware.tableparser.core.table.MovieParser._
    val mti: IO[Table[Movie]] = IOUsing(Source.fromURL(classOf[Movie].getResource("movie_metadata.csv")))(x => IO.fromTry(Table.parseSource(x)))
    implicit val hasKey: HasKey[Movie] = (t: Movie) => t.production.country
    val wi: IO[Int] = for (mt <- mti) yield mt.filterByKey(_ == "New Zealand").size
    // TODO eliminate use of unsafe methods
    wi.unsafeRunSync() shouldBe 4
  }
}
