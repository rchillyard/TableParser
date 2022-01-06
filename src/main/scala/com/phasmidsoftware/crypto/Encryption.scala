package com.phasmidsoftware.crypto

import cats.effect.IO
import tsec.cipher.symmetric
import tsec.cipher.symmetric.jca.{AES128CTR, SecretKey}

import scala.util.Random

/**
  * Trait to deal with Encryption.
  *
  * It is general in nature, but has only been tested with JCA AES128CTR.
  *
  * @tparam A the underlying type of the encryption.
  */
trait Encryption[A] {
  /**
    * Generate a random String of the required length.
    *
    * @return an IO of String.
    */
  def genRawKey: IO[String]

  /**
    * Build a key from the given String.
    *
    * @param raw a String of the required length (typically the result of calling genRawKey).
    * @return an IO of SecretKey[A]
    */
  def buildKey(raw: String): IO[SecretKey[A]]

  /**
    * Encrypt a plain text String.
    *
    * @param key       the key with which to encrypt the plain text.
    * @param plaintext the plain text to encrypt.
    * @return an IO of CipherText[A].
    */
  def encrypt(key: SecretKey[A])(plaintext: String): IO[symmetric.CipherText[A]]

  /**
    * Decrypt the given cipher text.
    *
    * @param key    the key with which to decrypt the cipher text.
    * @param cipher an instance of CipherText[A].
    * @return an IO of String.
    */
  def decrypt(key: SecretKey[A])(cipher: symmetric.CipherText[A]): IO[String]
}

/**
  * An object which provides encryption based on AES128CTR.
  */
object EncryptAES128CTR extends Encryption[AES128CTR] {

  import tsec.cipher.common.padding.NoPadding
  import tsec.cipher.symmetric
  import tsec.cipher.symmetric.jca.CTR
  import tsec.cipher.symmetric.jca.primitive.JCAPrimitiveCipher
  import tsec.cipher.symmetric.{IvGen, PlainText}
  import tsec.common._

  implicit val ctrStrategy: IvGen[IO, AES128CTR] = AES128CTR.defaultIvStrategy[IO]
  implicit val cachedInstance: JCAPrimitiveCipher[IO, AES128CTR, CTR, NoPadding] = AES128CTR.genEncryptor[IO] //Cache the implicit

  val random: Random = new Random()

  def genRawKey: IO[String] = {
    // CONSIDER using Cats effect for Random.
    val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ$abcdefghijklmnopqrstuvwxyz_0123456789"
    val sb = new StringBuilder
    for (_ <- 0 to 15) sb.append(alphabet.charAt(random.nextInt(alphabet.length)))
    IO {
      sb.toString
    }
  }

  def buildKey(rawKey: String): IO[SecretKey[AES128CTR]] = for (key <- AES128CTR.buildKey[IO](rawKey.utf8Bytes)) yield key

  def encrypt(key: SecretKey[AES128CTR])(plaintext: String): IO[symmetric.CipherText[AES128CTR]] = AES128CTR.encrypt[IO](PlainText(plaintext.utf8Bytes), key)

  def decrypt(key: SecretKey[AES128CTR])(cipher: symmetric.CipherText[AES128CTR]): IO[String] = for (z <- AES128CTR.decrypt[IO](cipher, key)) yield z.toUtf8String
}