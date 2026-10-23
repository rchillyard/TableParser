package com.phasmidsoftware.tableparser.cats.table

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.tableparser.cats.util.EvaluateIO.matchIO
import com.phasmidsoftware.tableparser.cats.util.IOUsing
import com.phasmidsoftware.tableparser.core.examples.Movie
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
    import com.phasmidsoftware.tableparser.core.examples.Movie._

    val mti: IO[Table[Movie]] = IO.fromTry(Table.parseResource("movie_metadata.csv", classOf[Movie]))
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
    import com.phasmidsoftware.tableparser.core.examples.Movie._
    var errorString = ""
    val errorFunc: Throwable => IO[Unit] = e => IO{errorString = e.getLocalizedMessage}

    val mti: IO[Table[Movie]] = IO.fromTry(Table.parseResource("movie_metadataX.csv")).onError { case e => errorFunc(e) }
    // TODO eliminate use of unsafe
    mti.attempt.unsafeRunSync() should matchPattern { case Left(_) => }
    errorString shouldBe "Table.sourceFromClassResource: cannot find resource 'movie_metadataX.csv' relative to class com.phasmidsoftware.tableparser.core.table.Table$"
  }

  /**
   * NOTE: it is perfectly proper for there to be a number of parsing problems.
   * These are application-specific and are not indicative of any bugs in the
   * TableParser library itself.
   */
  // NOTE: the columns are those that Movie reads (all but facenumber_in_poster of the original columns), so what is written can be read back.
  it should "be ingested and written out properly" in {
    import com.phasmidsoftware.tableparser.core.examples.Movie._

    val mti: IO[Table[Movie]] = IOUsing(Source.fromURL(classOf[Movie].getResource("movie_metadata.csv")))(x => IO.fromTry(Table.parseSource(x)))

    val wi: IO[String] = mti flatMap (mt => IO.fromTry(mt.toCSV)) // for (mt <- mti) yield mt.toCSV
    // TODO eliminate use of unsafe methods
    wi.unsafeRunSync().startsWith(
      """movie_title,color,language,aspect_ratio,duration,country,budget,gross,title_year,imdb_score,movie_facebook_likes,content_rating,num_user_for_reviews,num_voted_users,num_critic_for_reviews,cast_total_facebook_likes,director_name,director_facebook_likes,actor_1_name,actor_1_facebook_likes,actor_2_name,actor_2_facebook_likes,actor_3_name,actor_3_facebook_likes,genres,plot_keywords,movie_imdb_link
        |Avatar,Color,English,1.78,178,USA,237000000,760505847,2009,7.9,33000,PG-13,3054,886204,723,4834,James Cameron,0,CCH Pounder,1000,Joel David Moore,936,Wes Studi,855,Action|Adventure|Fantasy|Sci-Fi,avatar|future|marine|native|paraplegic,http://www.imdb.com/title/tt0499549/?ref_=fn_tt_tt_1""".stripMargin.replace("\n", "\r\n")) shouldBe true
  }

  it should "parse and filter the movies from the IMDB dataset" in {
    import com.phasmidsoftware.tableparser.core.examples.Movie._
    val mti: IO[Table[Movie]] = IOUsing(Source.fromURL(classOf[Movie].getResource("movie_metadata.csv")))(x => IO.fromTry(Table.parseSource(x)))
    implicit val hasKey: HasKey[Movie] = (t: Movie) => t.production.country
    val wi: IO[Int] = for (mt <- mti) yield mt.filterByKey(_ == "New Zealand").size
    // TODO eliminate use of unsafe methods
    wi.unsafeRunSync() shouldBe 4
  }
}
