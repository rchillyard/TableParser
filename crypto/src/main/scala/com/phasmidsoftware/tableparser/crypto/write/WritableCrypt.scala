/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.crypto.write

import com.phasmidsoftware.tableparser.core.write.Writable
import com.phasmidsoftware.tableparser.crypto.HexEncryption
import java.io.{File, FileWriter}

/**
 * Trait to enable rendering of a table to a sequential (non-hierarchical) output format, with each row encrypted.
 * A typeclass.
 *
 * @tparam O the underlying type, for example, a FileWriter.
 */
trait WritableCrypt[O] extends Writable[O]:

  /**
   * Method to write a row, encrypted, followed by a newline: the key, then "|", then the encrypted row (as Hex).
   * This is the form that EncryptedHeadedStringTableParser reads.
   *
   * @param o         the instance of O whither the row should be written.
   * @param key       the lookup key for this row (not an encryption key): it is written in plain text.
   * @param rawKey    the encryption key for this row (for AES128CTR, 16 characters).
   * @param plaintext the plain text character sequence to be written in encrypted form.
   * @tparam A the cipher algorithm (for which there must be evidence of HexEncryption[A]).
   * @return an instance of O which represents the updated output structure.
   */
  def writeLineEncrypted[A: HexEncryption](o: O)(key: String, rawKey: String, plaintext: CharSequence): O =
    import cats.effect.unsafe.implicits.global
    // NOTE a Writable is synchronous, so each row's encryption (an IO) is run here.
    // TableCrypt.writeCSVFileEncryptedIO writes a whole table in IO, without this.
    val hex = summon[HexEncryption[A]].encryptHex(rawKey, plaintext).unsafeRunSync()
    writeRaw(writeRaw(o)(s"$key|$hex"))(newline)

object WritableCrypt:

  /**
   * A WritableCrypt which writes to the given file.
   * (Up to 2.0.1, there was no WritableCrypt at all, so an encrypted table could not be written.)
   *
   * @param file the file.
   * @return a WritableCrypt[FileWriter].
   */
  def fileWritableCrypt(file: File): WritableCrypt[FileWriter] =
    new WritableCrypt[FileWriter]:
      def unit: FileWriter = new FileWriter(file)

      def writeRaw(o: FileWriter)(x: CharSequence): FileWriter =
        o.append(x)
        o

      override def close(o: FileWriter): Unit = o.close()
