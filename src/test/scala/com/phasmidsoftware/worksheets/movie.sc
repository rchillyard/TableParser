import com.phasmidsoftware.parse.StringTableParser
import com.phasmidsoftware.table.MovieParser.MovieTableParser
import com.phasmidsoftware.table._
import com.phasmidsoftware.util.FP.resource

import scala.io.Source
import scala.util.{Failure, Success, Try}

// NOTE: See also the airbnb.sc which explains the first line of code.

// NOTE: We show how to parse the movie database where the resulting Table rows
// are of type Movie (a case class).
// The movie_metadata.csv file is in the resources directory in the same package as class Table.
val sy: Try[Source] = for (u <- resource[Table[_]]("movie_metadata.csv")) yield Source.fromURL(u)

// NOTE: this MovieTableParser is configured to read each row as a Movie,
// and to "forgive" any parsing errors.
val parser: StringTableParser[Table[Movie]] = MovieTableParser

// NOTE: parse the source and return a Try[Table[Movie]].
// The parsing errors will be listed in the console because of a logging issue (there are a lot of them!)
val ty: Try[Table[Movie]] = parser parse sy

ty match {
  case Success(t) => println(s"Successfully parsed ---${t.rows}--- movies")
  case Failure(_) =>
}
