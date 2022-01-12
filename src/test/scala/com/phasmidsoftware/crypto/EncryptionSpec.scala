package com.phasmidsoftware.crypto

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EncryptionSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Encryption"

  private val encryptor = EncryptionAES128CTR

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
        hex <- Encryption.bytesToHexString(bytes1)
        bytes2 <- Encryption.hexStringToBytes(hex)
        encrypted <- encryptor.bytesToCipherText(bytes2)
        message <- encryptor.decrypt(key)(encrypted)
      } yield message
    result.unsafeRunSync() shouldBe originalMessage
  }

}
