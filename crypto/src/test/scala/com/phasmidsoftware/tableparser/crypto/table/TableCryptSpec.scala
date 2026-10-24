package com.phasmidsoftware.tableparser.crypto.table

import com.phasmidsoftware.tableparser.core.parse.{CellParser, CellParsers, StandardRowParser, StringParser, TableParser}
import com.phasmidsoftware.tableparser.core.render.CsvProduct
import com.phasmidsoftware.tableparser.core.table.{HasKey, Table}
import com.phasmidsoftware.tableparser.crypto.parse.EncryptedHeadedStringTableParser
import com.phasmidsoftware.tableparser.crypto.{EncryptionUTF8AES128CTR, HexEncryption}
import java.io.File
import java.nio.file.Files
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success}
import tsec.cipher.symmetric.jca.AES128CTR

/**
 * Round trip: a table is written encrypted (by TableCrypt) and read back (by EncryptedHeadedStringTableParser).
 * Up to 2.0.1, nothing could write encrypted rows, and the parser didn't decrypt.
 */
class TableCryptSpec extends AnyFlatSpec with should.Matchers:

  import TableCryptSpec._

  private given HexEncryption[AES128CTR] = EncryptionUTF8AES128CTR

  private val keyMap = Map("A1" -> "k0JCcO$SY5OI50uj", "B2" -> "QwSeQVJNuAg6D6H9", "C3" -> "dTLsxr132eucgu10")

  private val accounts = Seq(Account("A1", "Ada", 100), Account("B2", "Bob", 2500), Account("C3", "Cy, Jr.", 7))

  private def tempFile: File =
    val f = File.createTempFile("accounts", ".csv")
    f.deleteOnExit()
    f

  behavior of "TableCrypt"

  it should "write a table encrypted, with the header in plain text and each row encrypted" in:
    val file = tempFile
    TableCrypt.writeCSVFileEncrypted[AES128CTR, Account](Table(accounts, None))(file.toPath, keyMap) shouldBe Success(())
    val lines = Files.readAllLines(file.toPath)
    lines.get(0) shouldBe "id,name,balance"
    lines.size shouldBe 4
    lines.get(1) should startWith("A1|")
    lines.get(1) should not include "Ada"

  it should "write a table which can be read back" in:
    val file = tempFile
    TableCrypt.writeCSVFileEncrypted[AES128CTR, Account](Table(accounts, None))(file.toPath, keyMap) shouldBe Success(())
    given TableParser[Table[Account]] = EncryptedHeadedStringTableParser[Account, AES128CTR](keyMap.contains, keyMap)
    Table.parseFile[Table[Account]](file.toPath).map(_.toSeq) shouldBe Success(accounts)

  it should "read back only the rows chosen" in:
    val file = tempFile
    TableCrypt.writeCSVFileEncrypted[AES128CTR, Account](Table(accounts, None))(file.toPath, keyMap) shouldBe Success(())
    given TableParser[Table[Account]] = EncryptedHeadedStringTableParser[Account, AES128CTR](_ == "B2", keyMap)
    Table.parseFile[Table[Account]](file.toPath).map(_.toSeq) shouldBe Success(Seq(Account("B2", "Bob", 2500)))

  it should "fail (visibly) if a row has no key" in:
    val file = tempFile
    TableCrypt.writeCSVFileEncrypted[AES128CTR, Account](Table(accounts, None))(file.toPath, keyMap - "C3") should matchPattern { case Failure(_) => }

  it should "fail to read a row with the wrong key" in:
    val file = tempFile
    TableCrypt.writeCSVFileEncrypted[AES128CTR, Account](Table(accounts, None))(file.toPath, keyMap) shouldBe Success(())
    val wrongKeys = keyMap + ("A1" -> "0123456789abcdef")
    given TableParser[Table[Account]] = EncryptedHeadedStringTableParser[Account, AES128CTR](_ == "A1", wrongKeys)
    Table.parseFile[Table[Account]](file.toPath).map(_.toSeq) should not be Success(Seq(accounts.head))

  behavior of "TableCrypt.writeCSVFileEncryptedIO"

  it should "write nothing until the IO is run" in:
    import cats.effect.unsafe.implicits.global
    val file = tempFile
    file.delete()
    val io = TableCrypt.writeCSVFileEncryptedIO[AES128CTR, Account](Table(accounts, None))(file.toPath, keyMap)
    file.exists() shouldBe false
    io.unsafeRunSync()
    file.exists() shouldBe true

  it should "write a table which can be read back, each line ending with CRLF" in:
    import cats.effect.unsafe.implicits.global
    val file = tempFile
    TableCrypt.writeCSVFileEncryptedIO[AES128CTR, Account](Table(accounts, None))(file.toPath, keyMap).unsafeRunSync()
    val text = Files.readString(file.toPath)
    text.split("\r\n", -1).length shouldBe 5
    EncryptedHeadedStringTableParser[Account, AES128CTR](keyMap.contains, keyMap).parseIO(text.linesIterator, 1).unsafeRunSync().toSeq shouldBe accounts

  it should "fail in the IO if a row has no key" in:
    import cats.effect.unsafe.implicits.global
    TableCrypt.writeCSVFileEncryptedIO[AES128CTR, Account](Table(accounts, None))(tempFile.toPath, keyMap - "C3").attempt.unsafeRunSync() should matchPattern { case Left(_: NoSuchElementException) => }

object TableCryptSpec:
  case class Account(id: String, name: String, balance: Int) derives CsvProduct

  object Account extends CellParsers:
    given HasKey[Account] = (a: Account) => a.id

    given CellParser[Account] = CellParser.derived[Account]

    given StringParser[Account] = StandardRowParser.create[Account]
