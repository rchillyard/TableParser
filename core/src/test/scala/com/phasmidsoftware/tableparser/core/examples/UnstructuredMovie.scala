package com.phasmidsoftware.tableparser.core.examples

import com.phasmidsoftware.tableparser.core.parse.ColumnHelper.camelToSnakeCaseColumnNameMapperLower
import com.phasmidsoftware.tableparser.core.parse._
import com.phasmidsoftware.tableparser.core.render._

/**
 * All 28 columns of the movie database (movie_metadata.csv) in one flat case class.
 * Compare Movie, which groups the same columns into nested case classes.
 *
 * The fields are in the same order as the columns, and their names are the column names in camelCase:
 * camelToSnakeCaseColumnNameMapperLower maps, e.g., numCriticForReviews to num_critic_for_reviews and actor3Name to actor_3_name.
 * Numeric columns which can be blank are optional.
 *
 * 1.x could not do this: its cellParserN methods stopped at 13 fields, and its rendererN and generatorN methods at 19.
 * In any case, they took a constructor function, and Scala 2 had no functions or tuples of more than 22 elements.
 * Scala 3 has no such limit, and the derived instances don't use constructor functions at all.
 */
case class UnstructuredMovie(color: String,
                             directorName: String,
                             numCriticForReviews: Option[Int],
                             duration: Option[Int],
                             directorFacebookLikes: Option[Int],
                             actor3FacebookLikes: Option[Int],
                             actor2Name: String,
                             actor1FacebookLikes: Option[Int],
                             gross: Option[Long],
                             genres: String,
                             actor1Name: String,
                             movieTitle: String,
                             numVotedUsers: Option[Int],
                             castTotalFacebookLikes: Option[Int],
                             actor3Name: String,
                             facenumberInPoster: Option[Int],
                             plotKeywords: String,
                             movieImdbLink: String,
                             numUserForReviews: Option[Int],
                             language: String,
                             country: String,
                             contentRating: String,
                             budget: Option[Long],
                             titleYear: Option[Int],
                             actor2FacebookLikes: Option[Int],
                             imdbScore: Double,
                             aspectRatio: Option[Double],
                             movieFacebookLikes: Option[Int]) derives CsvProduct

object UnstructuredMovie extends CellParsers with CsvRenderers {
  implicit val helper: ColumnHelper[UnstructuredMovie] = columnHelper(camelToSnakeCaseColumnNameMapperLower)

  // A missing Double renders as nothing (the default renderer of Option[Double] writes "no double"), so that a row renders as it was read.
  implicit val optionDoubleRenderer: CsvRenderer[Option[Double]] = optionRenderer[Double]()

  implicit val parser: CellParser[UnstructuredMovie] = CellParser.derived[UnstructuredMovie]

  // NOTE: unlike Movie, this uses the default RowConfig, whose lists must be enclosed in "{}".
  // So the genres and plot keywords (e.g. Action|Adventure) are read as plain Strings, not as lists.

  implicit val stringParser: StringParser[UnstructuredMovie] = StandardRowParser.create[UnstructuredMovie]

  implicit object UnstructuredMovieTableParser extends HeadedCSVTableParser[UnstructuredMovie] {
    override val forgiving: Boolean = true
  }
}
