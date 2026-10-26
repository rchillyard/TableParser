package com.phasmidsoftware.tableparser.core

import com.phasmidsoftware.tableparser.core.parse._
import com.phasmidsoftware.tableparser.core.render.CsvProduct
import com.phasmidsoftware.tableparser.core.table._
import java.nio.file.{Files, Path}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Success, Try}

/**
 * The examples of the Getting Started guide in README.md, each as it appears there.
 * If one of these tests has to change, so must the README.
 */
class ReadmeSpec extends AnyFlatSpec with Matchers:

  import ReadmeSpec._

  behavior of "Getting Started"

  it should "parse a first table" in:
    import FirstTable._
    val path = file(pioneersCsv)
    // From here, as in the README.
    val pioneers: Try[Table[Pioneer]] = Table.parseFile[Table[Pioneer]](path)
    pioneers.map(_.size) shouldBe Success(5)
    pioneers.map(_.head) shouldBe Success(Pioneer("Ada Lovelace", 1815, Some(1852), "the first published program"))
    pioneers.map(_.last.died) shouldBe Success(None)
    pioneers.map(_.toSeq(3).achievement) shouldBe Success("structured programming, \"goto considered harmful\"")

  it should "map field names to column names" in:
    import ColumnNames._
    val pioneers = Table.parseFile[Table[Pioneer]](file(pioneersCsvTitled))
    pioneers.map(_.head) shouldBe Success(Pioneer("Ada Lovelace", 1815, Some(1852), "the first published program"))

  it should "parse nested case classes" in:
    import Nested._
    val pioneers = Table.parseFile[Table[Pioneer]](file(pioneersCsv))
    pioneers.map(_.head) shouldBe Success(Pioneer("Ada Lovelace", Life(1815, Some(1852)), "the first published program"))

  it should "leave out rows which can't be parsed, if forgiving" in:
    import Forgiving._
    val pioneers = Table.parseFile[Table[Pioneer]](file(pioneersCsv + "Charles Babbage,1791,eighteen seventy-one,the Analytical Engine\r\n"))
    pioneers.map(_.size) shouldBe Success(5)
    // Without forgiveness, the whole table fails.
    Table.parseFile[Table[FirstTable.Pioneer]](file(pioneersCsv + "Charles Babbage,1791,eighteen seventy-one,the Analytical Engine\r\n")).isFailure shouldBe true

  it should "read tab-separated values" in:
    import TabSeparated._
    val pioneers = Table.parseFile[Table[Pioneer]](file(pioneersCsv.replace(',', '\t').replace("\"structured programming\t \"\"goto considered harmful\"\"\"", "structured programming")))
    pioneers.map(_.size) shouldBe Success(5)
    pioneers.map(_.head.name) shouldBe Success("Ada Lovelace")

  it should "ignore spaces next to the delimiters, with trimmingRowConfig" in:
    import Trimming._
    val pioneers = Table.parseFile[Table[Pioneer]](file(pioneersCsv.replace(",", ", ")))
    pioneers.map(_.head) shouldBe Success(Pioneer("Ada Lovelace", 1815, Some(1852), "the first published program"))

  it should "work with a table" in:
    import FirstTable._
    val pioneers: Table[Pioneer] = Table.parseFile[Table[Pioneer]](file(pioneersCsv)).get
    // From here, as in the README.
    val living: Table[Pioneer] = pioneers.filter(_.died.isEmpty)
    val ages: Table[(String, Int)] = pioneers.mapOptional(p => p.died.map(d => p.name -> (d - p.born)))
    val firstTwo: Table[Pioneer] = pioneers.take(2)
    val byBirth: Seq[Pioneer] = pioneers.toSeq.sortBy(_.born)
    living.map(_.name).toSeq shouldBe Seq("Barbara Liskov")
    ages.toSeq.head shouldBe ("Ada Lovelace" -> 37)
    firstTwo.size shouldBe 2
    byBirth.map(_.born) shouldBe Seq(1815, 1906, 1912, 1930, 1939)
    pioneers.maybeColumnNames shouldBe Some(Seq("name", "born", "died", "achievement"))

  it should "write a table as CSV" in:
    import Writing._
    val pioneers: Table[Pioneer] = Table.parseFile[Table[Pioneer]](file(pioneersCsv)).get
    // From here, as in the README.
    val csv: Try[String] = pioneers.take(2).toCSV
    csv shouldBe Success("name,born,died,achievement\r\nAda Lovelace,1815,1852,the first published program\r\nAlan Turing,1912,1954,\"computability, the Turing machine\"\r\n")
    val out = Files.createTempFile("pioneers", ".csv")
    out.toFile.deleteOnExit()
    pioneers.writeCSVFile(out)
    // A table that is written can be read back.
    Table.parseFile[Table[Pioneer]](out).map(_.toSeq) shouldBe Success(pioneers.toSeq)

  it should "explore an unknown file" in:
    val path = file(pioneersCsv)
    // From here, as in the README.
    val raw: Try[RawTable] = Table.parsePathRaw(path, TableParser.includeAll)
    raw.map(_.head.ws) shouldBe Success(Seq("Ada Lovelace", "1815", "1852", "the first published program"))
    raw.flatMap(_.head("achievement")) shouldBe Success("the first published program")
    val statistics: ColumnStatistics = Analysis.forCsv(path)
    statistics.rows shouldBe 5
    statistics.columns shouldBe 4

  it should "select rows by RFC 7111" in:
    import FirstTable._
    val uri = file(pioneersCsv).toUri
    // From here, as in the README (row 1 is the header).
    val two: Try[Table[Pioneer]] = Table.parse[Table[Pioneer]](java.net.URI.create(s"$uri#row=3-4"))
    two.map(_.map(_.name).toSeq) shouldBe Success(Seq("Alan Turing", "Grace Hopper"))

object ReadmeSpec:

  // pioneers.csv, as in the README.
  val pioneersCsv: String = Seq(
    "name,born,died,achievement"
    , "Ada Lovelace,1815,1852,the first published program"
    , "Alan Turing,1912,1954,\"computability, the Turing machine\""
    , "Grace Hopper,1906,1992,the first compiler"
    , "Edsger Dijkstra,1930,2002,\"structured programming, \"\"goto considered harmful\"\"\""
    , "Barbara Liskov,1939,,the substitution principle"
  ).map(_ + "\r\n").mkString

  // The same, with other column names.
  val pioneersCsvTitled: String =
    pioneersCsv.replace("name,born,died,achievement", "Full Name,Year of Birth,Year of Death,Achievement")

  // Writes w to a temporary file.
  def file(w: String): Path =
    val path = Files.createTempFile("pioneers", ".csv")
    path.toFile.deleteOnExit()
    Files.writeString(path, w)

  object FirstTable:
    case class Pioneer(name: String, born: Int, died: Option[Int], achievement: String)

    object Pioneer extends TableParserHelper[Pioneer]():
      def cellParser: CellParser[Pioneer] = CellParser.derived[Pioneer]

  object ColumnNames:
    case class Pioneer(name: String, yearOfBirth: Int, yearOfDeath: Option[Int], achievement: String)

    object Pioneer extends TableParserHelper[Pioneer]():
      given ColumnHelper[Pioneer] = columnHelper(ColumnHelper.camelCaseColumnNameMapperSpace, "name" -> "Full Name")

      def cellParser: CellParser[Pioneer] = CellParser.derived[Pioneer]

  object Nested:
    case class Life(born: Int, died: Option[Int])

    case class Pioneer(name: String, life: Life, achievement: String)

    object Pioneer extends TableParserHelper[Pioneer]():
      def cellParser: CellParser[Pioneer] = CellParser.derived[Pioneer]

  object Forgiving:
    case class Pioneer(name: String, born: Int, died: Option[Int], achievement: String)

    object Pioneer extends TableParserHelper[Pioneer](forgiving = true):
      def cellParser: CellParser[Pioneer] = CellParser.derived[Pioneer]

  object TabSeparated:
    case class Pioneer(name: String, born: Int, died: Option[Int], achievement: String)

    object Pioneer extends CellParsers:
      given RowConfig = RowConfig(delimiter = '\t')
      given CellParser[Pioneer] = CellParser.derived[Pioneer]
      given StringParser[Pioneer] = StandardRowParser.create[Pioneer]

      given PioneerTableParser: HeadedCSVTableParser[Pioneer] = new HeadedCSVTableParser[Pioneer] {}

  object Trimming:
    case class Pioneer(name: String, born: Int, died: Option[Int], achievement: String)

    object Pioneer extends CellParsers:
      given RowConfig = RowConfig.trimmingRowConfig
      given CellParser[Pioneer] = CellParser.derived[Pioneer]
      given StringParser[Pioneer] = StandardRowParser.create[Pioneer]

      given PioneerTableParser: HeadedCSVTableParser[Pioneer] = new HeadedCSVTableParser[Pioneer] {}

  object Writing:
    case class Pioneer(name: String, born: Int, died: Option[Int], achievement: String) derives CsvProduct

    object Pioneer extends TableParserHelper[Pioneer]():
      def cellParser: CellParser[Pioneer] = CellParser.derived[Pioneer]
