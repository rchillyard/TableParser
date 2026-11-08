package com.phasmidsoftware.tableparser.core

import com.phasmidsoftware.tableparser.core.parse.{CellParser, PlainTextHeadedStringTableParser, TableParserHelper}
import com.phasmidsoftware.tableparser.core.table.Table
import java.nio.file.{Files, Path}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Success, Try, Using}

/**
 * The streaming example of the Getting Started guide in README.md (reading the rows of a large file with core alone),
 * as it appears there. If this test has to change, so must the README.
 * (The cats example which follows it in the README is tested in ReadmeStreamSpec, in the cats module.)
 */
class ReadmeRowsSpec extends AnyFlatSpec with Matchers:

  import ReadmeRowsSpec._

  behavior of "Getting Started: large files"

  it should "read the rows of a file as they are parsed" in:
    val path = file()
    // From here, as in the README (with path for Paths.get("pioneers.csv")).
    val parser = PlainTextHeadedStringTableParser[Pioneer]()
    val living: Try[List[String]] = for
      rows <- Table.rowsFile(parser)(path)
      names <- Using(rows)(_.collect { case Success(p) if p.died.isEmpty => p.name }.toList)
    yield names
    living shouldBe Success(List("Barbara Liskov"))

  behavior of "User Guide: Streaming, without an effect library"

  it should "count the rows which match, as they are parsed (the same as parsing the whole table)" in:
    import com.phasmidsoftware.tableparser.core.examples.Movie
    import com.phasmidsoftware.tableparser.core.examples.Movie.MovieTableParser
    // As in the README (with rowsResource for rowsFile).
    val usa: Try[Int] = for
      rows <- Table.rowsResource(MovieTableParser)("movie_metadata.csv", classOf[Movie])
      n <- Using(rows)(_.count { case Success(m) => m.production.country == "USA"; case _ => false })
    yield n
    usa.isSuccess shouldBe true
    usa shouldBe Table.parseResource[Table[Movie]]("movie_metadata.csv", classOf[Movie]).map(_.count(_.production.country == "USA"))

object ReadmeRowsSpec:
  case class Pioneer(name: String, born: Int, died: Option[Int], achievement: String)

  object Pioneer extends TableParserHelper[Pioneer]():
    def cellParser: CellParser[Pioneer] = CellParser.derived[Pioneer]

  // pioneers.csv, as in the README.
  def file(): Path =
    val path = Files.createTempFile("pioneers", ".csv")
    path.toFile.deleteOnExit()
    Files.writeString(path, Seq(
      "name,born,died,achievement"
      , "Ada Lovelace,1815,1852,the first published program"
      , "Alan Turing,1912,1954,\"computability, the Turing machine\""
      , "Grace Hopper,1906,1992,the first compiler"
      , "Edsger Dijkstra,1930,2002,\"structured programming, \"\"goto considered harmful\"\"\""
      , "Barbara Liskov,1939,,the substitution principle"
    ).map(_ + "\r\n").mkString)
