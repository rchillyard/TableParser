package com.phasmidsoftware.tableparser.crypto.render

import com.phasmidsoftware.tableparser.crypto.HexEncryption
import com.phasmidsoftware.tableparser.crypto.write.WritableCrypt
import com.phasmidsoftware.tableparser.core.render.{CsvGenerator, CsvRenderer, CsvTableRenderer}
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.write.Writable
import java.io.{File, FileWriter}

/**
 * Case class to help render a Table to a File in CSV format, with each row encrypted (see WritableCrypt).
 * The header is written in plain text.
 *
 * @param file          the file to which the table will be written.
 * @param keyFunction   a function which takes the key of a row (see HasKey) and returns the encryption key for that row
 *                      (the same function that EncryptedHeadedStringTableParser uses to decrypt it).
 * @param csvAttributes implicit instance of CsvAttributes.
 * @tparam T the type of object to be rendered, must provide evidence of CsvRenderer[T], CsvGenerator[T] and HasKey[T].
 * @tparam A the cipher algorithm (for which there must be evidence of HexEncryption[A]).
 */
case class CsvTableEncryptedFileRenderer[T: CsvRenderer : CsvGenerator : HasKey, A: HexEncryption](file: File, keyFunction: String => String)(using csvAttributes: CsvAttributes) extends CsvTableRenderer[T, FileWriter]()(using summon[CsvRenderer[T]], summon[CsvGenerator[T]], WritableCrypt.fileWritableCrypt(file), csvAttributes):
  override protected def generateText(ow: Writable[FileWriter], tc: CsvRenderer[T], o: FileWriter, t: T): FileWriter =
    val key = summon[HasKey[T]].key(t)
    val rendering = tc.render(t, Map())
    ow match
      case owC: WritableCrypt[FileWriter] => owC.writeLineEncrypted[A](o)(key, keyFunction(key), rendering)
      case _ => throw TableException("cannot render encrypted to non-encrypted Writable")
