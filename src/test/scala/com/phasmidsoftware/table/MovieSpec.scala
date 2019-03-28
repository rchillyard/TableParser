package com.phasmidsoftware.table

import com.phasmidsoftware.parse.{RowParser, TableParser}
import org.scalatest.{FlatSpec, Matchers}

import scala.util._

//noinspection SpellCheckingInspection
class MovieSpec extends FlatSpec with Matchers {

  behavior of "Movie table"

  it should "parse the first movie from the IMDB dataset" in {
    import MovieParser._

    val movies = Seq(
      "color,director_name,num_critic_for_reviews,duration,director_facebook_likes,actor_3_facebook_likes,actor_2_name,actor_1_facebook_likes,gross,genres,actor_1_name,movie_title,num_voted_users,cast_total_facebook_likes,actor_3_name,facenumber_in_poster,plot_keywords,movie_imdb_link,num_user_for_reviews,language,country,content_rating,budget,title_year,actor_2_facebook_likes,imdb_score,aspect_ratio,movie_facebook_likes",
      "Color,James Cameron,723,178,0,855,Joel David Moore,1000,760505847,Action|Adventure|Fantasy|Sci-Fi,CCH Pounder,Avatar,886204,4834,Wes Studi,0,avatar|future|marine|native|paraplegic,http://www.imdb.com/title/tt0499549/?ref_=fn_tt_tt_1,3054,English,USA,PG-13,237000000,2009,936,7.9,1.78,33000"
    )

    val x: Try[Table[Movie]] = Table.parse(movies)
    x should matchPattern { case Success(TableWithoutHeader(_)) => }
    val mt = x.get
    println(s"Movie: successfully parsed ${mt.size} rows")
    println(mt)
    mt.size shouldBe 1
  }

  // TODO rework this test to be more significant
  it should "parse the first (edited) movie from the IMDB dataset" in {
    import MovieParser._

    val movies = Seq(
      "color,director_name,num_critic_for_reviews,duration,director_facebook_likes,actor_3_facebook_likes,actor_2_name,actor_1_facebook_likes,gross,genres,actor_1_name,movie_title,num_voted_users,cast_total_facebook_likes,actor_3_name,facenumber_in_poster,plot_keywords,movie_imdb_link,num_user_for_reviews,language,country,content_rating,budget,title_year,actor_2_facebook_likes,imdb_score,aspect_ratio,movie_facebook_likes",
      "Color,James Cameron,,178,0,855,Joel David Moore,1000,760505847,Action|Adventure|Fantasy|Sci-Fi,CCH Pounder,Avatar,886204,4834,Wes Studi,0,avatar|future|marine|native|paraplegic,http://www.imdb.com/title/tt0499549/?ref_=fn_tt_tt_1,3054,English,USA,PG-13,,2009,936,7.9,1.78,33000"
    )

    val x: Try[Table[Movie]] = Table.parse(movies)
    x should matchPattern { case Success(TableWithoutHeader(_)) => }
    x.get.size shouldBe 1
  }

  // TODO rework this test
  ignore should "fail to parse the first (edited) movie from the IMDB dataset" in {
    import MovieParser._

    implicit object MovieTableParser extends TableParser[Table[Movie]] {
      type Row = Movie

      def hasHeader: Boolean = true

      override def forgiving: Boolean = false

      def rowParser: RowParser[Row] = implicitly[RowParser[Row]]

      def builder(rows: Seq[Row]): Table[Movie] = TableWithoutHeader(rows)
    }

    val movies = Seq(
      "color,director_name,num_critic_for_reviews,duration,director_facebook_likes,actor_3_facebook_likes,actor_2_name,actor_1_facebook_likes,gross,genres,actor_1_name,movie_title,num_voted_users,cast_total_facebook_likes,actor_3_name,facenumber_in_poster,plot_keywords,movie_imdb_link,num_user_for_reviews,language,country,content_rating,budget,title_year,actor_2_facebook_likes,imdb_score,aspect_ratio,movie_facebook_likes",
      "Color,James Cameron,,178,0,855,Joel David Moore,1000,760505847,Action|Adventure|Fantasy|Sci-Fi,CCH Pounder,Avatar,886204,4834,Wes Studi,0,avatar|future|marine|native|paraplegic,http://www.imdb.com/title/tt0499549/?ref_=fn_tt_tt_1,3054,English,USA,PG-13,,2009,936,7.9,1.78,33000"
    )

    val x: Try[Table[Movie]] = Table.parse(movies)
    x should matchPattern { case Failure(_) => }
  }

  // FIXME
  ignore should "parse all the following rows" in {
    import MovieParser._

    implicit object MovieTableParser extends TableParser[Table[Movie]] {
      type Row = Movie

      def hasHeader: Boolean = true

      override def forgiving: Boolean = false

      def rowParser: RowParser[Row] = implicitly[RowParser[Row]]

      def builder(rows: Seq[Row]): Table[Movie] = TableWithoutHeader(rows)
    }

    val movies = Seq(
      "color,director_name,num_critic_for_reviews,duration,director_facebook_likes,actor_3_facebook_likes,actor_2_name,actor_1_facebook_likes,gross,genres,actor_1_name,movie_title,num_voted_users,cast_total_facebook_likes,actor_3_name,facenumber_in_poster,plot_keywords,movie_imdb_link,num_user_for_reviews,language,country,content_rating,budget,title_year,actor_2_facebook_likes,imdb_score,aspect_ratio,movie_facebook_likes",
      ",Doug Walker,,,131,,Rob Walker,131,,Documentary,Doug Walker,Star Wars: Episode VII - The Force Awakens             ,8,143,,0,,http://www.imdb.com/title/tt5289954/?ref_=fn_tt_tt_1,,,,,,,12,7.1,,0"
    )

    Table.parse(movies) should matchPattern { case Success(TableWithoutHeader(_)) => }
  }

}
