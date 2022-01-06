package com.phasmidsoftware.crypto

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EncryptionSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Encryption"

  private val encryptor = EncryptAES128CTR

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

}
