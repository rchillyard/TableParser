package com.phasmidsoftware.tableparser.cats.render

import com.phasmidsoftware.tableparser.cats.crypto.HexEncryption
import com.phasmidsoftware.tableparser.cats.write.WritableCrypt
import com.phasmidsoftware.tableparser.core.render.{CsvGenerator, CsvRenderer, CsvTableRenderer}
import com.phasmidsoftware.tableparser.core.table._
import com.phasmidsoftware.tableparser.core.write.Writable
import java.io.{File, FileWriter}

/**
 * Case class to help render a Table to a File in CSV format.
 *
 * TODO remove duplicate code
 *
 * TESTME
 *
 * @param file          the file to which the table will be written.
 * @param csvAttributes implicit instance of CsvAttributes.
 * @tparam T the type of object to be rendered, must provide evidence of CsvRenderer[T] amd CsvGenerator[T].
 * @tparam A the cipher algorithm (for which there must be evidence of HexEncryption[A]).
 */
case class CsvTableEncryptedFileRenderer[T: CsvRenderer : CsvGenerator : HasKey, A: HexEncryption](file: File)(implicit csvAttributes: CsvAttributes) extends CsvTableRenderer[T, FileWriter]()(implicitly[CsvRenderer[T]], implicitly[CsvGenerator[T]], Writable.fileWritable(file)) {
  override protected def generateText(ow: Writable[FileWriter], tc: CsvRenderer[T], o: FileWriter, t: T): FileWriter = {
    val key = implicitly[HasKey[T]].key(t)
    val rendering = tc.render(t, Map())
    ow match {
      case owC: WritableCrypt[FileWriter] => owC.writeLineEncrypted(o)(key, rendering)
      case _ => throw TableException("cannot render encrypted to non-encrypted Writable")
    }

  }
}
