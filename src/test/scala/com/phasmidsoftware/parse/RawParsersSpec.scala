/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.parse

import com.phasmidsoftware.table.{HeadedTable, RawTable, Table}
import org.scalatest.flatspec
import org.scalatest.matchers.should
import scala.util.{Success, Try}

class RawParsersSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "parse"

  it should "parse the raw strings" in {
    import RawParsers.WithHeaderRow._

    val rows = Seq(
      "color,director_name,num_critic_for_reviews,duration,director_facebook_likes,actor_3_facebook_likes,actor_2_name,actor_1_facebook_likes,gross,genres,actor_1_name,movie_title,num_voted_users,cast_total_facebook_likes,actor_3_name,facenumber_in_poster,plot_keywords,movie_imdb_link,num_user_for_reviews,language,country,content_rating,budget,title_year,actor_2_facebook_likes,imdb_score,aspect_ratio,movie_facebook_likes",
      ",Doug Walker,,,131,,Rob Walker,131,,Documentary,Doug Walker,Star Wars: Episode VII - The Force Awakens             ,8,143,,0,,http://www.imdb.com/title/tt5289954/?ref_=fn_tt_tt_1,,,,,,,12,7.1,,0"
    )

    val mty: Try[RawTable] = Table.parse(rows)
    mty should matchPattern { case Success(HeadedTable(_, _)) => }
    val stringSeqTable: RawTable = mty.get
    stringSeqTable.size shouldBe 1
    stringSeqTable.head(1) shouldBe "Doug Walker"
  }

}
