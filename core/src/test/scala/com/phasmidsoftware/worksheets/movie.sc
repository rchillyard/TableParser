import cats.effect.IO
import com.phasmidsoftware.parse.StringTableParser
import com.phasmidsoftware.table.MovieParser.MovieTableParser
import com.phasmidsoftware.table._
import com.phasmidsoftware.util.FP.resource
import scala.io.Source

// NOTE: See also the airbnb.sc which explains the first line of code.

// NOTE: We show how to parse the movie database where the resulting Table rows
// are of type Movie (a case class).
// The movie_metadata.csv file is in the resources directory in the same package as class Table.
val sy: IO[Source] = IO.fromTry(for (u <- resource[Table[_]]("movie_metadata.csv")) yield Source.fromURL(u))

// NOTE: this MovieTableParser is configured to read each row as a Movie,
// and to "forgive" any parsing errors.
val parser: StringTableParser[Table[Movie]] = MovieTableParser

// NOTE: parse the source and return a Try[Table[Movie]].
// The parsing errors will be listed in the console because of a logging issue (there are a lot of them!)
val mti: IO[Table[Movie]] = parser parse sy

val zi: IO[Unit] = mti map {
  t => println(s"Successfully parsed ---${t.content}--- movies")
}

import cats.effect.unsafe.implicits.global

zi.unsafeRunSync()
