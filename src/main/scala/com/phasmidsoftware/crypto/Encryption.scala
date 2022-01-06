package com.phasmidsoftware.crypto

import cats.effect.IO
import tsec.cipher.common.padding.NoPadding
import tsec.cipher.symmetric
import tsec.cipher.symmetric.jca.primitive.JCAPrimitiveCipher
import tsec.cipher.symmetric.jca.{AES128CTR, CTR, SecretKey}
import tsec.cipher.symmetric.{IvGen, PlainText}

trait Encryption[A] {
  def genKey(raw: String): IO[SecretKey[A]]

  def encrypt(key: SecretKey[A])(plaintext: String): IO[symmetric.CipherText[A]]

  def decrypt(key: SecretKey[A])(cipher: symmetric.CipherText[A]): IO[String]
}

object EncryptAES128CTR extends Encryption[AES128CTR] {

  import tsec.common._

  //Feel free to choose any of the default Cipher constructions.
  //For non-authenticated ciphers, we recommend AES-CTR

  val toEncrypt: Array[Byte] = "hi hello welcome to tsec".utf8Bytes

  implicit val ctrStrategy: IvGen[IO, AES128CTR] = AES128CTR.defaultIvStrategy[IO]
  implicit val cachedInstance: JCAPrimitiveCipher[IO, AES128CTR, CTR, NoPadding] = AES128CTR.genEncryptor[IO] //Cache the implicit

  def genKey(raw: String): IO[SecretKey[AES128CTR]] = AES128CTR.generateKey[IO]
  //    AES128CTR.buildKey(raw.utf8Bytes)

  def encrypt(key: SecretKey[AES128CTR])(plaintext: String): IO[symmetric.CipherText[AES128CTR]] = AES128CTR.encrypt[IO](PlainText(plaintext.utf8Bytes), key)

  def decrypt(key: SecretKey[AES128CTR])(cipher: symmetric.CipherText[AES128CTR]): IO[String] = for (z <- AES128CTR.decrypt[IO](cipher, key)) yield z.toUtf8String

  def onlyEncrypt(toEncrypt: String): IO[String] =
    for {
      key <- AES128CTR.generateKey[IO] //Generate our key
      encrypted <- AES128CTR.encrypt[IO](PlainText(toEncrypt.utf8Bytes), key) //Encrypt our message
      decrypted <- AES128CTR.decrypt[IO](encrypted, key)
    } yield decrypted.toUtf8String // "hi hello welcome to tsec!"

  //  /** You can also turn it into a singular array with the IV concatenated at the end */
  //  val onlyEncrypt2: IO[String] =
  //    for {
  //      key       <- AES128CTR.generateKey[IO]                        //Generate our key
  //      encrypted <- AES128CTR.encrypt[IO](PlainText(toEncrypt), key) //Encrypt our message
  //      array = encrypted.toConcatenated
  //      from      <- IO.fromEither(AES128CTR.ciphertextFromConcat(array))
  //      decrypted <- AES128CTR.decrypt[IO](from, key)
  //    } yield decrypted.toUtf8String // "hi hello welcome to tsec!"
  //
  //  /** An authenticated encryption and decryption */
  //  implicit val gcmstrategy: IvGen[IO, AES128GCM] = AES128GCM.defaultIvStrategy[IO]
  //  implicit val cachedAADEncryptor: AADEncryptor[IO, AES128GCM, SecretKey] = AES128GCM.genEncryptor[IO]
  //
  //  val aad: symmetric.AAD.Type = AAD("myAdditionalAuthenticationData".utf8Bytes)
  //  val encryptAAD: IO[String] =
  //    for {
  //      key       <- AES128GCM.generateKey[IO]                                    //Generate our key
  //      encrypted <- AES128GCM.encryptWithAAD[IO](PlainText(toEncrypt), key, aad) //Encrypt
  //      decrypted <- AES128GCM.decryptWithAAD[IO](encrypted, key, aad)            //Decrypt
  //    } yield decrypted.toUtf8String // "hi hello welcome to tsec!"
  //
  //  /** For more advanced usage, i.e you know which cipher you want specifically, you must import padding
  //    * as well as the low level package
  //    *
  //    * this is not recommended, but useful for.. science!
  //    *
  //    */
  //  import tsec.cipher.common.padding._
  //  import tsec.cipher.symmetric.jca.primitive._
  //  val desStrategy: IvGen[IO, DES] = JCAIvGen.random[IO, DES]
  //  implicit val instance: JCAPrimitiveCipher[IO, DES, CBC, PKCS7Padding] = JCAPrimitiveCipher.sync[IO, DES, CBC, PKCS7Padding]
  //
  //  val advancedUsage: IO[String] = for {
  //    key       <- DES.generateKey[IO]
  //    iv        <- desStrategy.genIv
  //    encrypted <- instance.encrypt(PlainText(toEncrypt), key, iv) //Encrypt our message, with our auth data
  //    decrypted <- instance.decrypt(encrypted, key) //Decrypt our message: We need to pass it the same AAD
  //  } yield decrypted.toUtf8String

}