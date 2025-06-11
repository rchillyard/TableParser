import com.phasmidsoftware.tableparser.core.examples.Movie
import com.phasmidsoftware.tableparser.core.examples.Movie.MovieTableParser
import com.phasmidsoftware.tableparser.core.parse.StringTableParser
import com.phasmidsoftware.tableparser.core.table.Table.sourceFromClassResource
import com.phasmidsoftware.tableparser.core.table._
import scala.io.Source
import scala.util.Try

// NOTE: We show how to parse the movie database where the resulting Table rows
// are of type Movie (a case class).
// The movie_metadata.csv file is in the resources directory in the same package as class Movie.
val sy: Try[Source] = sourceFromClassResource("movie_metadata.csv", classOf[Movie])

// NOTE: this MovieTableParser is configured to read each row as a Movie,
// and to "forgive" any parsing errors.
val parser: StringTableParser[Table[Movie]] = MovieTableParser

// NOTE: parse the source and return a Try[Table[Movie]].
val mty: Try[Table[Movie]] = parser parse sy

// Show the content of the table.
mty map {
  t => println(s"Successfully parsed ---${t.content}--- movies")
}
