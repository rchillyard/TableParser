package com.phasmidsoftware.tableparser.crypto.parse

import com.phasmidsoftware.tableparser.crypto.{EncryptionUTF8AES128CTR, HexEncryption}
import com.phasmidsoftware.tableparser.core.parse._
import com.phasmidsoftware.tableparser.core.table.{CsvFragment, Header, RawRow, RawTable, RowSelection, Table}
import org.scalatest.flatspec
import org.scalatest.matchers.should
import tsec.cipher.symmetric.jca.AES128CTR

class EncryptedHeadedStringTableParserSpec extends flatspec.AnyFlatSpec with should.Matchers:


  implicit val z: HexEncryption[AES128CTR] = EncryptionUTF8AES128CTR

  behavior of "EncryptedHeadedStringTableParser"
  it should "it should set header of encrypted parser" in:
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 2)
    the[TableParserException] thrownBy parser.setHeader(Header(Seq(Seq("a"))))

  behavior of "EncryptedHeadedStringTableParser (decryption)"

  // An encrypted table of team projects: two (plaintext) header lines, then five encrypted rows, keyed by team number.
  private val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")
  private val encryptedTable = "/com/phasmidsoftware/tableparser/cats/examples/teamproject/TeamProjectEncrypted.csv"

  private def lines: Iterator[String] = scala.io.Source.fromURL(getClass.getResource(encryptedTable)).getLines()

  private def rawParser(team: String): EncryptedHeadedStringTableParser[RawRow, AES128CTR] =
    given CellParser[RawRow] = RawParsers.WithHeaderRow.rawRowCellParser
    EncryptedHeadedStringTableParser[RawRow, AES128CTR](_ == team, keyMap, headerRowsToRead = 2)

  // Up to 2.0.1, parseIO dropped the first encrypted row (here, team 1's), so it found nothing.
  it should "decrypt the chosen rows with parseIO" in:
    import cats.effect.unsafe.implicits.global
    val table = rawParser("1").parseIO(lines, 2).unsafeRunSync()
    table.size shouldBe 1
    table.head.ws.take(5) shouldBe Seq("1", "Leonhard Euler", "Daniel Bernoulli", "Isaac Newton", "Srinivas Ramanujan")
    table.maybeHeader.map(_.xs.head) shouldBe Some("Team Number")

  // Up to 2.0.1, Table.parse (and parseFile, parseResource, etc.) didn't decrypt at all: they gave the encrypted rows.
  it should "decrypt the chosen rows with Table.parse" in:
    given TableParser[RawTable] = rawParser("2")
    val table = Table.parse[RawTable](lines).get
    table.size shouldBe 1
    table.head.ws.head shouldBe "2"
    table.head.ws.mkString should not include "|"

  behavior of "EncryptedHeadedStringTableParser setters (issue #112: up to 2.3.0, they gave a parser which didn't decrypt)"

  private def teams(parser: TableParser[RawTable]): scala.util.Try[Seq[String]] =
    given TableParser[RawTable] = parser
    Table.parse[RawTable](lines).map(_.toSeq.map(_.ws.head))

  private def allTeams: EncryptedHeadedStringTableParser[RawRow, AES128CTR] =
    given CellParser[RawRow] = RawParsers.WithHeaderRow.rawRowCellParser
    EncryptedHeadedStringTableParser[RawRow, AES128CTR](keyMap.contains, keyMap, headerRowsToRead = 2)

  it should "keep decrypting after setPredicate (applied to the decrypted rows)" in:
    val parser: EncryptedHeadedStringTableParser[RawRow, AES128CTR] = allTeams.setPredicate(_.map(_.ws.head != "3").getOrElse(false))
    teams(parser) shouldBe scala.util.Success(Seq("1", "2", "4", "5"))

  it should "keep decrypting after setPlaintextPredicate" in:
    teams(allTeams.setPlaintextPredicate(_.map(_.ws.head != "3").getOrElse(false))) shouldBe scala.util.Success(Seq("1", "2", "4", "5"))

  it should "keep decrypting after setRawPredicate (applied to the decrypted rows before they are parsed)" in:
    teams(allTeams.setRawPredicate(_.ws.head != "4")) shouldBe scala.util.Success(Seq("1", "2", "3", "5"))

  it should "keep decrypting after setForgiving, and be forgiving" in:
    val parser = allTeams.setForgiving(true)
    parser.forgiving shouldBe true
    teams(parser) shouldBe scala.util.Success(Seq("1", "2", "3", "4", "5"))

  it should "keep decrypting after setMultiline" in:
    teams(allTeams.setMultiline(true)) shouldBe scala.util.Success(Seq("1", "2", "3", "4", "5"))

  it should "keep decrypting after setRowParser" in:
    given CellParser[RawRow] = RawParsers.WithHeaderRow.rawRowCellParser
    teams(allTeams.setRowParser(StandardRowParser.create[RawRow])) shouldBe scala.util.Success(Seq("1", "2", "3", "4", "5"))

  it should "keep each setting when another is made" in:
    val parser = allTeams.setRawPredicate(_.ws.head != "4").setPredicate(_.map(_.ws.head != "3").getOrElse(false)).setMultiline(true)
    teams(parser) shouldBe scala.util.Success(Seq("1", "2", "5"))
    teams(rawParser("2").setPredicate(_ => true)) shouldBe scala.util.Success(Seq("2"))

  it should "decrypt every row whose key is chosen" in:
    given CellParser[RawRow] = RawParsers.WithHeaderRow.rawRowCellParser
    given TableParser[RawTable] = EncryptedHeadedStringTableParser[RawRow, AES128CTR](keyMap.contains, keyMap, headerRowsToRead = 2)
    Table.parse[RawTable](lines).map(_.toSeq.map(_.ws.head)) shouldBe scala.util.Success(Seq("1", "2", "3", "4", "5"))

  behavior of "EncryptedHeadedStringTableParser streaming (up to 2.3.0, it gave the encrypted rows)"

  it should "decrypt the chosen rows with IOTable.streamFile" in:
    import cats.effect.unsafe.implicits.global
    import com.phasmidsoftware.tableparser.cats.table.IOTable
    val path = java.nio.file.Paths.get(getClass.getResource(encryptedTable).toURI)
    val wsy = scala.util.Try(IOTable.streamFile(rawParser("1"))(path).compile.toList.unsafeRunSync())
    wsy.map(_.map(_.ws.head)) shouldBe scala.util.Success(Seq("1"))

  it should "decrypt the chosen rows with parseSelected" in:
    rawParser("2").parseSelected(lines, RowSelection.all, 2).map(_.toSeq.map(_.ws.head)) shouldBe scala.util.Success(Seq("2"))

  it should "stream every chosen row, with the settings of the setters" in:
    import cats.effect.unsafe.implicits.global
    import com.phasmidsoftware.tableparser.cats.table.IOTable
    val parser = allTeams.setRawPredicate(_.ws.head != "4").setPredicate(_.map(_.ws.head != "3").getOrElse(false))
    IOTable.streamLines(parser)(lines.toSeq).map(_.ws.head).compile.toList.unsafeRunSync() shouldBe List("1", "2", "5")

  it should "select rows by their lines in the encrypted input" in:
    // the two header lines are rows 1 and 2, so team 2's row is row 4.
    allTeams.parseSelected(lines, CsvFragment.parse("row=4-5").collect { case rs: RowSelection => rs }.get, 2).map(_.toSeq.map(_.ws.head)) shouldBe scala.util.Success(Seq("2", "3"))

  it should "end with a Failure at a row which can't be decrypted" in:
    given CellParser[RawRow] = RawParsers.WithHeaderRow.rawRowCellParser
    val parser = EncryptedHeadedStringTableParser[RawRow, AES128CTR](_ => true, keyMap - "3", headerRowsToRead = 2)
    val rys = parser.rows(lines, 2).get._2.toList
    rys.take(2).map(_.map(_.ws.head)) shouldBe List(scala.util.Success("1"), scala.util.Success("2"))
    rys.size shouldBe 3
    rys.last.isFailure shouldBe true
