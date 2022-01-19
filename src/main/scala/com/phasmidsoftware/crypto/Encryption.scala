package com.phasmidsoftware.crypto

import cats.effect.IO
import com.phasmidsoftware.crypto.Encryption.hexStringToBytes
import tsec.cipher.symmetric
import tsec.cipher.symmetric.jca.{AES128CTR, SecretKey}

import scala.util.Random

/**
  * Trait to deal with Encryption.
  *
  * It is general in nature, but has only been tested with JCA AES128CTR.
  *
  * @tparam A the cipher algorithm.
  */
trait Encryption[A] {

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

  /**
    * Show the given cipher text as a an array of bytes.
    *
    * @param cipher an instance of CipherText[A].
    * @return an IO of Array[Byte].
    */
  def concat(cipher: symmetric.CipherText[A]): IO[Array[Byte]]

  /**
    * THe inverse of concat.
    *
    * @param bytes the byte array.
    * @return an IO of CipherText[A].
    */
  def bytesToCipherText(bytes: Array[Byte]): IO[symmetric.CipherText[A]]

  /**
    * Given a raw key and a Hex string, do the decryption.
    *
    * @param rawKey a raw key, i.e. a sequence of 16 characters.
    * @param hex    a string of Hexadecimal digits representing a cipher.
    * @return the decrypted String, wrapped in IO.
    */
  def decryptHex(rawKey: String, hex: String): IO[String] =
    for {
      x <- buildKey(rawKey)
      bytes <- Encryption.hexStringToBytes(hex)
      cipher <- bytesToCipherText(bytes)
      y <- decrypt(x)(cipher)
    } yield y

  /**
    * Method to check that the given Hex String really does decrypt to the given plaintext.
    *
    * @param hex       a String of hexadecimals.
    * @param key       the secret key.
    * @param plaintext the original plain text.
    * @return true if the Hex string is correct.
    */
  def checkHex(hex: String, key: SecretKey[A], plaintext: String): IO[Boolean] = for {
    bytes <- hexStringToBytes(hex)
    encrypted <- bytesToCipherText(bytes)
    message <- decrypt(key)(encrypted)
    ok = message == plaintext
  } yield ok
}

object Encryption {

  /**
   * Show the given bytes as hexadecimal text.
   *
   * @param bytes an Array[Byte].
   * @return an IO of String.
   */
  def bytesToHexString(bytes: Array[Byte]): IO[String] = {
    val sb = new StringBuilder
    for (b <- bytes) yield sb.append(String.format("%02X", b))
    IO(sb.toString())
  }

  def hexStringToBytes(hex: String): IO[Array[Byte]] = {
    // CONSIDER getting the byte array a different way that doesn't require the drop.
    val q = BigInt(hex, 16).toByteArray
    val bytes = if (q.length > hex.length / 2) q.drop(1) else q
    IO(bytes)
  }

  /**
    * Method to decrypt a row consisting of an identifier and a Hex string.
    *
    * @param keyFunction a function to yield the raw cipher key from the value of the ID column
    *                    (said value might well be ignored).
    * @param row         a two-element sequence of Strings: the id and the hex string.
    *                    * @tparam A the cipher algorithm.
    * @tparam A the cipher algorithm (for which there must be evidence of Encryption[A]).
    * @return a IO[String].
    */
  def decryptRow[A: Encryption](keyFunction: String => String)(row: Seq[String]): IO[String] = {
    val ko = row.headOption // the first element of the row is the identifier (row-id).
    val hexIndex = 1 // the second (and last) element in the row is the Hex string.
    val f = row.lift
    (for (key <- ko map keyFunction; hex <- f(hexIndex)) yield (key, hex)) match {
      case Some(key -> hex) => implicitly[Encryption[A]].decryptHex(key, hex)
      case _ => throw new RuntimeException(s"Encryption.decryptRow: logic error")
    }
  }

  val random: Random = new Random()

  def genRawKey: IO[String] = {
    // CONSIDER using Cats effect for Random.
    val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ-abcdefghijklmnopqrstuvwxyz_0123456789"
    val sb = new StringBuilder
    for (_ <- 0 to 15) sb.append(alphabet.charAt(random.nextInt(alphabet.length)))
    IO(sb.toString)
  }
}

/**
 * An object which provides encryption based on AES128CTR.
 */
object EncryptionAES128CTR extends Encryption[AES128CTR] {

  import tsec.cipher.common.padding.NoPadding
  import tsec.cipher.symmetric
  import tsec.cipher.symmetric.jca.CTR
  import tsec.cipher.symmetric.jca.primitive.JCAPrimitiveCipher
  import tsec.cipher.symmetric.{IvGen, PlainText}
  import tsec.common._

  implicit val ctrStrategy: IvGen[IO, AES128CTR] = AES128CTR.defaultIvStrategy[IO]
  implicit val cachedInstance: JCAPrimitiveCipher[IO, AES128CTR, CTR, NoPadding] = AES128CTR.genEncryptor[IO] //Cache the implicit

  def buildKey(rawKey: String): IO[SecretKey[AES128CTR]] =
    if (rawKey.length == AES128CTR.keySizeBytes)
      for (key <- AES128CTR.buildKey[IO](rawKey.utf8Bytes)) yield key
    else throw new RuntimeException(s"buildKey: incorrect key size (should be ${AES128CTR.keySizeBytes})")

  def encrypt(key: SecretKey[AES128CTR])(plaintext: String): IO[symmetric.CipherText[AES128CTR]] =
    AES128CTR.encrypt[IO](PlainText(plaintext.utf8Bytes), key)

  def decrypt(key: SecretKey[AES128CTR])(cipher: symmetric.CipherText[AES128CTR]): IO[String] =
    for (z <- AES128CTR.decrypt[IO](cipher, key)) yield z.toUtf8String

  def concat(cipher: symmetric.CipherText[AES128CTR]): IO[Array[Byte]] = IO(cipher.toConcatenated)

  def bytesToCipherText(bytes: Array[Byte]): IO[symmetric.CipherText[AES128CTR]] = IO.fromEither(AES128CTR.ciphertextFromConcat(bytes))
}