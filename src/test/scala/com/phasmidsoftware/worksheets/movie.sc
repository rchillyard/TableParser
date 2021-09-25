import com.phasmidsoftware.parse.{StringTableParser, TableParser}
import com.phasmidsoftware.table.MovieParser.MovieTableParser
import com.phasmidsoftware.table.{Movie, Table}

import scala.io.Source
import scala.util.{Success, Try}

val source: Source = Source.fromURL(classOf[Table[_]].getResource("movie_metadata.csv"))

type MovieTable = Table[Movie]

val parser: StringTableParser[MovieTable] = MovieTableParser

import com.phasmidsoftware.parse.TableParser._

val z = 3 times "Hello"
//val z = implicitly[Parse[MovieTable]]

//val ty: Try[Table[Movie]] = parser.parse(source)

val ty: Try[MovieTable] = parser.parse(source)

ty match {
  case Success(t) => println(t.rows)
}

