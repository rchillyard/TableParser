package com.phasmidsoftware.tableparser.core.parse

import com.phasmidsoftware.tableparser.core.examples.Movie
import com.phasmidsoftware.tableparser.core.table.{HeadedTable, Table}
import com.phasmidsoftware.tableparser.core.util.EvaluateTry.matchTry
import org.scalatest.flatspec
import org.scalatest.matchers.should

import scala.io.BufferedSource
import scala.util.{Success, Try}

class ImplicitParserSpec extends flatspec.AnyFlatSpec with should.Matchers:

  import scala.io.Source

  behavior of "implicit class"

  // NOTE movie_metadata_small.csv is the header and first 20 rows of movie_metadata.csv.
  // Neither test here asserts anything about the number of rows, so the full set only costs time (#206).
  private val resourceName = "movie_metadata_small.csv"

  it should "properly parse movie data" in:
    val sy: Try[Source] = Try(Source.fromURL(classOf[Movie].getResource(resourceName)))
    val parser: StringTableParser[Table[Movie]] = summon[StringTableParser[Table[Movie]]]
    val parsed: Try[Table[Movie]] = sy flatMap (ms => parser.parse(ms.getLines()))
    matchTry(parsed):
      case tm@HeadedTable(_, _) =>
        println(s"Table read with ${tm.content} rows")
        succeed

  class MySource(s: Source) extends Source:
    protected val iter: Iterator[Char] = s match
      case b: BufferedSource => b.iter
      case _ => fail("wrong type of Source")
    var open: Boolean = true

    override def close(): Unit =
      super.close()
      open = false

  it should "properly close the source" in {
    val source = new MySource(Source.fromURL(classOf[Movie].getResource(resourceName)))
    source.open shouldBe true : Unit
    val parser: StringTableParser[Table[Movie]] = summon[StringTableParser[Table[Movie]]]
    val xy: Try[Table[Movie]] = parser parse Try(source)
    xy should matchPattern { case Success(_) => } : Unit
    source.open shouldBe false
  }
