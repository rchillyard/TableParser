package com.phasmidsoftware.tableparser.cats.table

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.tableparser.core.parse.{CellParser, PlainTextHeadedStringTableParser, TableParserHelper}
import java.nio.file.{Files, Path}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The streaming example of the Getting Started guide in README.md, as it appears there.
 * If this test has to change, so must the README.
 */
class ReadmeStreamSpec extends AnyFlatSpec with Matchers:

  import ReadmeStreamSpec._

  behavior of "Getting Started: large files"

  it should "stream the rows of a file" in:
    val path = file()
    // From here, as in the README.
    val parser = PlainTextHeadedStringTableParser[Pioneer]()
    val living: IO[List[String]] = IOTable.streamFile(parser)(path).filter(_.died.isEmpty).map(_.name).compile.toList
    living.unsafeRunSync() shouldBe List("Barbara Liskov")

object ReadmeStreamSpec:
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
