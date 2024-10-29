package com.phasmidsoftware.tableparser.cats.parse

import com.phasmidsoftware.tableparser.cats.crypto.{EncryptionUTF8AES128CTR, HexEncryption}
import com.phasmidsoftware.tableparser.core.parse._
import com.phasmidsoftware.tableparser.core.table.Header
import org.scalatest.flatspec
import org.scalatest.matchers.should
import tsec.cipher.symmetric.jca.AES128CTR

class EncryptedHeadedStringTableParserSpec extends flatspec.AnyFlatSpec with should.Matchers {


  implicit val z: HexEncryption[AES128CTR] = EncryptionUTF8AES128CTR

  behavior of "EncryptedHeadedStringTableParser"
  it should "it should set header of encrypted parser" in {
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 2)
    the[TableParserException] thrownBy parser.setHeader(Header(Seq(Seq("a"))))
  }

  it should "it should set predicate of plaintext" in {
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 2)
    parser.setPredicate(TableParser.sampler(2)) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1)
  }

  it should "it should set forgiving of encrypted parser" in {
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 2)
    parser.setForgiving(true) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = true, 2)
  }

  it should "it should set multiline of encrypted parser" in {
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 1)
    parser.setMultiline(true) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1)
  }

  it should "it should set plaintext predicate of encrypted parser" in {
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 1)
    parser.setPlaintextPredicate(TableParser.sampler(2)) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1)
  }

  it should "it should set row parser of EncryptedHeadedStringTableParser" in {
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj", "2" -> "QwSeQVJNuAg6D6H9", "3" -> "dTLsxr132eucgu10", "4" -> "mexd0Ta81di$fCGp", "5" -> "cb0jlsf4DXtZz_kf")

    def encryptionPredicate(w: String): Boolean = w == "1" // We only decrypt for team 1's row

    val parser = EncryptedHeadedStringTableParser[Int, AES128CTR](encryptionPredicate, keyMap, headerRowsToRead = 1)
    val rowConfig = RowConfig.defaultEncryptedRowConfig
    val lineParser: LineParser = LineParser.apply(rowConfig)
    parser.setRowParser(StandardRowParser[Int](lineParser)) shouldBe PlainTextHeadedStringTableParser[Int](None, forgiving = false, 1)
  }


}
