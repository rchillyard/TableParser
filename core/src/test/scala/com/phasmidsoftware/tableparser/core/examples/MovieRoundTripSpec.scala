package com.phasmidsoftware.tableparser.core.examples

import com.phasmidsoftware.tableparser.core.table.{HeadedTable, Table}
import com.phasmidsoftware.tableparser.core.util.EvaluateTry.matchTry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Try

/**
 * A table of movies which is written as CSV can be read back, giving the same movies.
 * This needs the columns that are written to be the ones that are read: for the nested types (e.g. Principal,
 * whose ColumnHelper has the prefix "$x_$c"), and for the types that are parsed from one cell (Name, Rating and AttributeSet).
 */
class MovieRoundTripSpec extends AnyFlatSpec with Matchers:

  import Movie._

  behavior of "Movie (round trip)"

  it should "read back the movies that it writes" in:
    val result: Try[(Table[Movie], Table[Movie])] = for {
      movies <- Table.parseResource[Table[Movie]]("movie_metadata.csv", classOf[Movie])
      csv <- movies.toCSV
      again <- Table.parse[Table[Movie]](csv.linesIterator.toSeq)
    } yield (movies, again)

    matchTry(result):
      case (movies@HeadedTable(_, _), again@HeadedTable(_, _)) =>
        movies.size shouldBe 1567
        again.size shouldBe movies.size
        again.toSet shouldBe movies.toSet

  it should "write only columns of the original file" in:
    matchTry(for (movies <- Table.parseResource[Table[Movie]]("movie_metadata.csv", classOf[Movie]); csv <- movies.toCSV) yield csv.linesIterator.next()):
      case header =>
        val original = Movie.header.split(",").toSet
        header.split(",").filterNot(original) shouldBe empty
