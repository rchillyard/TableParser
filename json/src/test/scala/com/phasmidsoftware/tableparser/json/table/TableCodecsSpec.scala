package com.phasmidsoftware.tableparser.json.table

import com.phasmidsoftware.tableparser.core.examples.UnstructuredMovie
import com.phasmidsoftware.tableparser.core.render.CsvProduct
import com.phasmidsoftware.tableparser.core.table.{HeadedTable, Header, Table}
import com.phasmidsoftware.tableparser.core.util.EvaluateTry.matchTry
import com.phasmidsoftware.tableparser.json.render.JsonTableRenderer
import com.phasmidsoftware.tableparser.json.table.TableCodecs.given
import io.circe.parser.decode
import io.circe.syntax.*
import io.circe.{Codec, DecodingFailure, ParsingFailure}
import java.time.LocalDate
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.io.Source
import scala.util.Using

class TableCodecsSpec extends AnyFlatSpec with should.Matchers:

  import TableCodecsSpec._

  behavior of "TableCodecs with the movie database (CSV to JSON and back)"

  // UnstructuredMovie (in core's tests) has all 28 columns of movie_metadata.csv, many of them optional.
  private given Codec.AsObject[UnstructuredMovie] = Codec.AsObject.derived

  private lazy val (csvHeader, avatarLine) = Using.resource(Source.fromURL(classOf[UnstructuredMovie].getResource("movie_metadata.csv"))) { s =>
    val lines = s.getLines()
    (lines.next(), lines.next())
  }

  private lazy val movies: Table[UnstructuredMovie] =
    Table.parseResource[Table[UnstructuredMovie]]("movie_metadata.csv", classOf[UnstructuredMovie]).get

  private lazy val json: String = new JsonTableRenderer[UnstructuredMovie].render(movies)

  it should "read back every movie exactly as it was written" in:
    val decoded = decode[Table[UnstructuredMovie]](json)
    decoded.map(_.size) shouldBe Right(1609)
    decoded.map(_.toList) shouldBe Right(movies.toList)
    decoded.map(_.maybeHeader) shouldBe Right(movies.maybeHeader)

  it should "write the header as the column names, and the rows with the field names" in:
    // The header comes from the CSV file, so its names are the column names (e.g. director_name);
    // the rows are encoded by their Codec, so their keys are the field names (e.g. directorName).
    movies.maybeHeader.map(_.xs.mkString(",")) shouldBe Some(csvHeader)
    json should include(""""director_name",""")
    json should include(""""directorName" : "James Cameron",""")

  it should "write a missing value as null" in:
    // Some movies have no gross (a blank in the CSV file).
    movies.toList.exists(_.gross.isEmpty) shouldBe true
    json should include(""""gross" : null,""")

  it should "render a movie read back from JSON as the line it was read from" in:
    matchTry(decode[Table[UnstructuredMovie]](json).toTry):
      case mt@HeadedTable(_, _) =>
        mt.find(_.movieTitle.startsWith("Avatar")) map (summon[CsvProduct[UnstructuredMovie]].render(_)) shouldBe Some(avatarLine)

  behavior of "TableCodecs with other types"

  it should "encode numbers, dates, options and nested case classes" in:
    val table = Table(people, Some(Header.create("name", "born", "height", "children", "address")))
    table.asJson.noSpaces shouldBe
            """{"rows":[""" +
                    """{"name":"Ann","born":"1990-02-28","height":1.65,"children":2,"address":{"street":"1 High St","zip":2134}},""" +
                    """{"name":"Bob","born":"1985-12-01","height":1.8,"children":0,"address":{"street":"2 Low Rd","zip":null}},""" +
                    """{"name":"Cy","born":"2001-07-04","height":1.72,"children":1,"address":null}""" +
                    """],"header":["name","born","height","children","address"]}"""

  it should "decode numbers, dates, options and nested case classes" in:
    val table = Table(people, None)
    decode[Table[Person]](table.asJson.spaces2).map(_.toList) shouldBe Right(people)

  it should "decode a missing optional field as None" in:
    decode[Table[Person]]("""{"rows": [{"name": "Cy", "born": "2001-07-04", "height": 1.72, "children": 1}]}""").map(_.toList) shouldBe
            Right(List(Person("Cy", LocalDate.of(2001, 7, 4), 1.72, 1, None)))

  behavior of "TableCodecs with bad input"

  it should "fail to decode malformed JSON" in:
    decode[Table[Person]]("""{"rows": [""") should matchPattern { case Left(_: ParsingFailure) => }

  it should "fail to decode a row with a missing (non-optional) field" in:
    val result = decode[Table[Person]]("""{"rows": [{"name": "Cy", "height": 1.72, "children": 1}]}""")
    result should matchPattern { case Left(_: DecodingFailure) => }
    result.left.map(_.getMessage) should matchPattern { case Left(w: String) if w.contains("born") => }

  it should "fail to decode a row with a value of the wrong type" in:
    decode[Table[Person]]("""{"rows": [{"name": "Cy", "born": "July 4th", "height": 1.72, "children": 1}]}""") should matchPattern { case Left(_: DecodingFailure) => }

  it should "fail to decode rows which are not an array" in:
    decode[Table[Person]]("""{"rows": 42}""") should matchPattern { case Left(_: DecodingFailure) => }

  it should "fail to decode a header which is not an array of strings" in:
    decode[Table[Person]]("""{"rows": [], "header": "name"}""") should matchPattern { case Left(_: DecodingFailure) => }

  it should "decode an object with no rows as an empty table" in:
    decode[Table[Person]]("""{}""").map(_.size) shouldBe Right(0)

object TableCodecsSpec:
  case class Address(street: String, zip: Option[Int]) derives Codec.AsObject

  case class Person(name: String, born: LocalDate, height: Double, children: Int, address: Option[Address]) derives Codec.AsObject

  val people: List[Person] = List(
    Person("Ann", LocalDate.of(1990, 2, 28), 1.65, 2, Some(Address("1 High St", Some(2134))))
    , Person("Bob", LocalDate.of(1985, 12, 1), 1.8, 0, Some(Address("2 Low Rd", None)))
    , Person("Cy", LocalDate.of(2001, 7, 4), 1.72, 1, None)
  )
