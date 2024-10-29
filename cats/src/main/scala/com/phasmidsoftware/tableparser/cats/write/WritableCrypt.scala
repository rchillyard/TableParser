/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.cats.write

import cats.effect.IO
import com.phasmidsoftware.tableparser.cats.crypto.HexEncryption
import com.phasmidsoftware.tableparser.core.write.Writable

/**
 * Trait to enable rendering of a table to a sequential (non-hierarchical) output format.
 * A typeclass.
 *
 * @tparam O the underlying type, for example, a StringBuilder.
 */
trait WritableCrypt[O] extends Writable[O] {

  /**
   * Method to write a character sequence to the given instance o followed by a newline.
   *
   * @param o         the instance of O whither the parameter x should be written.
   * @param key       the lookup key for this row (not an encryption key).
   * @param plaintext the plain text character sequence to be written in encrypted form.
   * @tparam A the cipher algorithm (for which there must be evidence of Encryption[A]).
   * @return an instance of O which represents the updated output structure.
   */
  def writeLineEncrypted[A: HexEncryption](o: O)(key: String, plaintext: CharSequence): O = {
    import cats.effect.unsafe.implicits.global
    val encryption = implicitly[HexEncryption[A]]
    val wBi: IO[(String, String, Boolean)] = encryption.encryptWithRandomKey(plaintext)

    // TODO use IO for O
    wBi.unsafeRunSync() match {
      case (k, w, true) =>
        println(s"$key: $k") // CONSIDER writing these key/password pairs to the log file.
        writeRaw(writeRaw(o)(s"$key|$w"))(newline)
      case x => throw new RuntimeException(s"Writable.writeLineEncrypted: logic error: wBi=$x, plaintext=$plaintext")
    }
  }

}
