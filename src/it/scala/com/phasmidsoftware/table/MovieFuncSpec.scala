package com.phasmidsoftware.table

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.parse.{AttributeSet, StringList}
import com.phasmidsoftware.render.{CsvGenerators, CsvRenderer, CsvRenderers}
import com.phasmidsoftware.util.IOUsing
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.io.Source

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

  // TODO move this into Movie
  class CsvRendererMovie(implicit val csvAttributes: CsvAttributes) extends CsvRenderers with CsvRenderer[Movie] {

    import com.phasmidsoftware.render.CsvRenderers._

    implicit val rendererStringList: CsvRenderer[StringList] = sequenceRenderer[String]
    implicit val rendererOptionDouble: CsvRenderer[Option[Double]] = optionRenderer
    implicit val rendererOptionInt: CsvRenderer[Option[Int]] = optionRenderer
    implicit val rendererOptionString: CsvRenderer[Option[String]] = optionRenderer
    implicit val rendererFormat: CsvRenderer[Format] = renderer4(Format)
    implicit val rendererProduction: CsvRenderer[Production] = renderer4(Production)
    implicit val rendererRating: CsvRenderer[Rating] = renderer2(Rating.apply)
    implicit val rendererReviews: CsvRenderer[Reviews] = renderer7(Reviews)
    implicit val rendererName: CsvRenderer[Name] = renderer4(Name.apply)
    implicit val rendererPrincipal: CsvRenderer[Principal] = renderer2(Principal)
    implicit val rendererOptionPrincipal: CsvRenderer[Option[Principal]] = optionRenderer
    val fAttributeSet: StringList => AttributeSet = AttributeSet.apply
    implicit val rendererAttributeSet: CsvRenderer[AttributeSet] = renderer1(fAttributeSet)

    def render(t: Movie, attrs: Map[String, String]): String = renderer11(Movie).render(t, attrs)
  }

  /**
   * NOTE: it is perfectly proper for there to be a number of parsing problems.
   * These are application-specific and are not indicative of any bugs in the
   * TableParser library itself.
   */
  it should "be ingested and written out properly" in {
    import MovieParser._

    val mti: IO[Table[Movie]] = IOUsing(Source.fromURL(classOf[Movie].getResource("movie_metadata.csv")))(x => Table.parseSource(x))
    implicit val csvRenderer: CsvRenderer[Movie] = new CsvRendererMovie()
    implicit val csvGenerator: CsvGenerator[Movie] = createMovieCvsGenerator

    val wi: IO[String] = for (mt <- mti) yield mt.toCSV
    wi.unsafeRunSync().startsWith(
      """title,format.color,format.language,format.aspectRatio,format.duration,production.country,production.budget,production.gross,production.titleYear,reviews.imdbScore,reviews.facebookLikes,reviews.contentRating.code,reviews.contentRating.age,reviews.numUsersReview,reviews.numUsersVoted,reviews.numCriticReviews,reviews.totalFacebookLikes,director.name.first,director.name.middle,director.name.last,director.name.suffix,director.facebookLikes,actor1.name.first,actor1.name.middle,actor1.name.last,actor1.name.suffix,actor1.facebookLikes,actor2.name.first,actor2.name.middle,actor2.name.last,actor2.name.suffix,actor2.facebookLikes,actor3,genres.xs,plotKeywords.xs,imdb
        |Avatar,Color,English,1.78,178,USA,237000000,760505847,2009,7.9,33000,PG,13,3054,886204,723,4834,James,,Cameron,,0,CCH,,Pounder,,1000,Joel,David,Moore,,936,Wes,,Studi,,855,Action,Adventure,Fantasy,Sci-Fi,avatar,future,marine,native,paraplegic,http://www.imdb.com/title/tt0499549/?ref_=fn_tt_tt_1""".stripMargin) shouldBe true
  }

  it should "parse and filter the movies from the IMDB dataset" in {
    import MovieParser._
    val mti: IO[Table[Movie]] = IOUsing(Source.fromURL(classOf[Movie].getResource("movie_metadata.csv")))(x => Table.parseSource(x))
    implicit val hasKey: HasKey[Movie] = (t: Movie) => t.production.country
    val wi: IO[Int] = for (mt <- mti) yield mt.filterByKey(_ == "New Zealand").size
    wi.unsafeRunSync() shouldBe 4
  }

  // TODO move this into Movie
  private def createMovieCvsGenerator: CsvGenerator[Movie] = {
    import com.phasmidsoftware.render.CsvGenerators._
    val csvGenerators = new CsvGenerators {}
    implicit val generatorStringList: CsvGenerator[StringList] = csvGenerators.sequenceGenerator[String]
    implicit val generatorOptionDouble: CsvGenerator[Option[Double]] = csvGenerators.optionGenerator
    implicit val generatorOptionInt: CsvGenerator[Option[Int]] = csvGenerators.optionGenerator
    implicit val generatorOptionString: CsvGenerator[Option[String]] = csvGenerators.optionGenerator
    implicit val generatorFormat: CsvGenerator[Format] = csvGenerators.generator4(Format)
    implicit val generatorProduction: CsvGenerator[Production] = csvGenerators.generator4(Production)
    implicit val generatorRating: CsvGenerator[Rating] = csvGenerators.generator2(Rating.apply)
    implicit val generatorReviews: CsvGenerator[Reviews] = csvGenerators.generator7(Reviews)
    implicit val generatorName: CsvGenerator[Name] = csvGenerators.generator4(Name.apply)
    implicit val generatorPrincipal: CsvGenerator[Principal] = csvGenerators.generator2(Principal)
    implicit val generatorOptionPrincipal: CsvGenerator[Option[Principal]] = csvGenerators.optionGenerator
    val fAttributeSet: StringList => AttributeSet = AttributeSet.apply
    implicit val generatorAttributeSet: CsvGenerator[AttributeSet] = csvGenerators.generator1(fAttributeSet)
    csvGenerators.generator11(Movie)
  }
}
