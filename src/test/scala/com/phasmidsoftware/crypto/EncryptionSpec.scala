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
//           _ = println(s"bytes length: ${bytes.length}, bytes: ${Arrays.toString(bytes)}")
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
//        _ = println(s"bytes1 length: ${bytes1.length}, bytes: ${Arrays.toString(bytes1)}")
        hex <- Encryption.bytesToHexString(bytes1)
//        _ = println(s"hex length: ${hex.length}, hex: $hex")
        bytes2 <- Encryption.hexStringToBytes(hex)
//        _ = println(s"bytes2 length: ${bytes2.length}, bytes: ${Arrays.toString(bytes2)}")
        encrypted <- encryptor.bytesToCipherText(bytes2)
        message <- encryptor.decrypt(key)(encrypted)
      } yield message
    result.unsafeRunSync() shouldBe originalMessage
  }

}
