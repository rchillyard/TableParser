package com.phasmidsoftware.crypto

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.parse.TableParserException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import tsec.cipher.symmetric.jca.AES128CTR

class EncryptionSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Encryption"

  implicit val encryptor: HexEncryption[AES128CTR] = EncryptionUTF8AES128CTR

  it should "buildKey" in {
    val key = for {
      rawKey <- encryptor.genRawKey
    } yield encryptor.buildKey(rawKey)
    key.unsafeRunSync()
  }

  it should "encrypt and decrypt" in {
    val originalMessage = "Hello, World!"
    val result: IO[String] = for {
      rawKey <- encryptor.genRawKey
      _ = println(rawKey)
      key <- encryptor.buildKey(rawKey)
      ciphertext <- encryptor.encrypt(key)(originalMessage)
      message <- encryptor.decrypt(key)(ciphertext)
    } yield message
    val z = result.unsafeRunSync()
    z shouldBe originalMessage
  }

  it should "round-trip via Byte array" in {
    val originalMessage = "Hello, World!"
    val result: IO[String] =
      for {
        rawKey <- encryptor.genRawKey
        key <- encryptor.buildKey(rawKey)
        ciphertext <- encryptor.encrypt(key)(originalMessage)
        bytes <- encryptor.concat(ciphertext)
        encrypted <- encryptor.bytesToCipherText(bytes)
        message <- encryptor.decrypt(key)(encrypted)
      } yield message
    result.unsafeRunSync() shouldBe originalMessage
  }

  it should "round-trip via Hex string" in {
    val originalMessage = "Hello, World!"
    val result: IO[String] =
      for {
        rawKey <- encryptor.genRawKey
        key <- encryptor.buildKey(rawKey)
        ciphertext <- encryptor.encrypt(key)(originalMessage)
        bytes1 <- encryptor.concat(ciphertext)
        hex <- HexEncryption.bytesToHexString(bytes1)
        bytes2 <- HexEncryption.hexStringToBytes(hex)
        encrypted <- encryptor.bytesToCipherText(bytes2)
        message <- encryptor.decrypt(key)(encrypted)
      } yield message
    result.unsafeRunSync() shouldBe originalMessage
  }

  it should "decryptHex" in {
    val encryption = implicitly[HexEncryption[AES128CTR]]
    val result = encryption.decryptHex("k0JCcO$SY5OI50uj", "391F322E2DE3EAF7C9029DB2BB873C3B1E60FD1F657B97DB17031B8774A21EE45E2740DC65246C0FA712290AE8255406BDA708D166029E80F4B31236AC33A6D43A09370196D43191715E9817A9846D66DF7E159BDC641344AE7196AEAD9CC44FF7F8D2A33A3D153D7ADC8DBD3312381896BEAC462EF4DEB4C05F502DE312994EA9D679E3825593291C4CFEBFC653F3121DC3FDA2FCDB80E7C072D7EC95942BFAD9EFD7ACCF51BA38D96A4E3A325C860FFA47C94093751B58C4A9A257931876F2ADEEEFF4C3E4662339D5F3066CB625B9EB0E508AB6C4FD950CA259BCC6EC4283AB9521758B5E7D1CAE4BFE852B76BDD3F390C2C6CF65BB15FDB5B91CEB5F6D6AF4FCC8318AF911BDA27E5419225D3B5274D4AF5C75D0E1089F94")
    result.unsafeRunSync() shouldBe "1,Leonhard Euler,Daniel Bernoulli,Isaac Newton,Srinivas Ramanujan,92.0,8.5,8.0,5.0,10.5,5.0,4.0,8.0,5.0,23.0,10.0,5.0,Presentation long and detailed.  Project excellent overall. Need to actually run UI myself.,https://github.com/youngbai/CSYE7200-MovieRecommendation"
  }

  import cats.effect.IO

  it should "decryptRow1" in {
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj")
    val row = Seq("1", "391F322E2DE3EAF7C9029DB2BB873C3B1E60FD1F657B97DB17031B8774A21EE45E2740DC65246C0FA712290AE8255406BDA708D166029E80F4B31236AC33A6D43A09370196D43191715E9817A9846D66DF7E159BDC641344AE7196AEAD9CC44FF7F8D2A33A3D153D7ADC8DBD3312381896BEAC462EF4DEB4C05F502DE312994EA9D679E3825593291C4CFEBFC653F3121DC3FDA2FCDB80E7C072D7EC95942BFAD9EFD7ACCF51BA38D96A4E3A325C860FFA47C94093751B58C4A9A257931876F2ADEEEFF4C3E4662339D5F3066CB625B9EB0E508AB6C4FD950CA259BCC6EC4283AB9521758B5E7D1CAE4BFE852B76BDD3F390C2C6CF65BB15FDB5B91CEB5F6D6AF4FCC8318AF911BDA27E5419225D3B5274D4AF5C75D0E1089F94")
    val wi: IO[String] = HexEncryption.decryptRow(keyMap)(row)
    wi.unsafeRunSync() shouldBe "1,Leonhard Euler,Daniel Bernoulli,Isaac Newton,Srinivas Ramanujan,92.0,8.5,8.0,5.0,10.5,5.0,4.0,8.0,5.0,23.0,10.0,5.0,Presentation long and detailed.  Project excellent overall. Need to actually run UI myself.,https://github.com/youngbai/CSYE7200-MovieRecommendation"
  }

  it should "decryptRow2" in {
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj")
    val row = Seq("2", "391F322E2DE3EAF7C9029DB2BB873C3B1E60FD1F657B97DB17031B8774A21EE45E2740DC65246C0FA712290AE8255406BDA708D166029E80F4B31236AC33A6D43A09370196D43191715E9817A9846D66DF7E159BDC641344AE7196AEAD9CC44FF7F8D2A33A3D153D7ADC8DBD3312381896BEAC462EF4DEB4C05F502DE312994EA9D679E3825593291C4CFEBFC653F3121DC3FDA2FCDB80E7C072D7EC95942BFAD9EFD7ACCF51BA38D96A4E3A325C860FFA47C94093751B58C4A9A257931876F2ADEEEFF4C3E4662339D5F3066CB625B9EB0E508AB6C4FD950CA259BCC6EC4283AB9521758B5E7D1CAE4BFE852B76BDD3F390C2C6CF65BB15FDB5B91CEB5F6D6AF4FCC8318AF911BDA27E5419225D3B5274D4AF5C75D0E1089F94")
    a[NoSuchElementException] shouldBe thrownBy(HexEncryption.decryptRow(keyMap)(row))
  }

  it should "decryptRow2a" in {
    val keyMap = Map("1" -> "k0JCcO$SY5OI50uj")
    val row = Seq("2", "391F322E2DE3EAF7C9029DB2BB873C3B1E60FD1F657B97DB17031B8774A21EE45E2740DC65246C0FA712290AE8255406BDA708D166029E80F4B31236AC33A6D43A09370196D43191715E9817A9846D66DF7E159BDC641344AE7196AEAD9CC44FF7F8D2A33A3D153D7ADC8DBD3312381896BEAC462EF4DEB4C05F502DE312994EA9D679E3825593291C4CFEBFC653F3121DC3FDA2FCDB80E7C072D7EC95942BFAD9EFD7ACCF51BA38D96A4E3A325C860FFA47C94093751B58C4A9A257931876F2ADEEEFF4C3E4662339D5F3066CB625B9EB0E508AB6C4FD950CA259BCC6EC4283AB9521758B5E7D1CAE4BFE852B76BDD3F390C2C6CF65BB15FDB5B91CEB5F6D6AF4FCC8318AF911BDA27E5419225D3B5274D4AF5C75D0E1089F94")
    a[NoSuchElementException] shouldBe thrownBy(HexEncryption.decryptRow(keyMap)(row).unsafeRunSync())
  }

  it should "decryptRow3" in {
    val keyMap = Map("1" -> "QwSeQVJNuAg6D6H9")
    val row = Seq("1")
    val wi: IO[String] = HexEncryption.decryptRow(keyMap)(row)
    a[TableParserException] shouldBe thrownBy(wi.unsafeRunSync())
  }

}
