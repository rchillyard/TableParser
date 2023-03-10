package com.phasmidsoftware.crypto

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import com.phasmidsoftware.parse.TableParserException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import tsec.cipher.symmetric.jca.AES128CTR

class EncryptionUTF8AES128CTRFuncSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Encryption"

  implicit val encryptor: HexEncryption[AES128CTR] = EncryptionUTF8AES128CTR

  it should "buildKey" in {
    val key = for {
      rawKey <- encryptor.genRawKey
    } yield encryptor.buildKey(rawKey)
    key.unsafeRunSync()
  }
}
