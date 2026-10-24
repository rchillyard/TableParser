package com.phasmidsoftware.tableparser.crypto.parse

import com.phasmidsoftware.tableparser.crypto.{EncryptionUTF8AES128CTR, HexEncryption}
import com.phasmidsoftware.tableparser.core.parse._
import com.phasmidsoftware.tableparser.core.table.{Header, RawRow, RawTable, Table}
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

  it should "it should set predicate of plaintext" in:
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 2)
    parser.setPredicate(TableParser.sampler(2)) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1)

  it should "it should set forgiving of encrypted parser" in:
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 2)
    parser.setForgiving(true) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = true, 2)

  it should "it should set multiline of encrypted parser" in:
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 1)
    parser.setMultiline(true) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1)

  it should "it should set plaintext predicate of encrypted parser" in:
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 1)
    parser.setPlaintextPredicate(TableParser.sampler(2)) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1)

  it should "it should set row parser of EncryptedHeadedStringTableParser" in:
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 1)
    val rowConfig = RowConfig.defaultEncryptedRowConfig
    val lineParser: LineParser = LineParser.apply(using rowConfig)
    parser.setRowParser(StandardRowParser[Int](lineParser)) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1)

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

  it should "decrypt every row whose key is chosen" in:
    given CellParser[RawRow] = RawParsers.WithHeaderRow.rawRowCellParser
    given TableParser[RawTable] = EncryptedHeadedStringTableParser[RawRow, AES128CTR](keyMap.contains, keyMap, headerRowsToRead = 2)
    Table.parse[RawTable](lines).map(_.toSeq.map(_.ws.head)) shouldBe scala.util.Success(Seq("1", "2", "3", "4", "5"))
