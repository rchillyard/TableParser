package com.phasmidsoftware.crypto

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import tsec.cipher.symmetric.jca.AES128CTR

class EncryptionSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Encrypt"

  val encryptor: Encryption[AES128CTR] = EncryptAES128CTR

  it should "onlyEncrypt" in {
    val originalMessage = "Hello, World!"
    val result: IO[String] = for {
      key <- encryptor.genKey("1234")
      ciphertext <- encryptor.encrypt(key)(originalMessage)
      message <- encryptor.decrypt(key)(ciphertext)
    } yield message
    result.unsafeRunSync() shouldBe originalMessage
  }

}
