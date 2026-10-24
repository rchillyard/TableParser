/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.crypto.table

import com.phasmidsoftware.tableparser.crypto.HexEncryption
import com.phasmidsoftware.tableparser.crypto.render.CsvTableEncryptedFileRenderer
import com.phasmidsoftware.tableparser.core.render._
import com.phasmidsoftware.tableparser.core.table.{CsvAttributes, HasKey, Table}
import java.nio.file.Path
import scala.language.postfixOps
import scala.util.Try

object TableCrypt:

  /**
   * Method to write a Table[Row] as a CSV file, with the header in plain text and each row encrypted
   * (as EncryptedHeadedStringTableParser reads it).
   *
   * @param table       the table.
   * @param path        the Path where the output should be stored.
   * @param keyFunction a function which takes the key of a row (see HasKey) and returns the encryption key for that row
   *                    (for AES128CTR, 16 characters).
   * @param renderer    implicit value of CsvRenderer[Row].
   * @param generator   implicit value of CsvProductGenerator[Row].
   * @param hasKey      implicit value of HasKey[Row].
   *                    This relates to a column which is the "key" column in a CSV (used for identification).
   *                    It is not directly related to cryptography.
   * @tparam A the cipher algorithm (for which there must be evidence of HexEncryption[A]).
   * @param csvAttributes implicit value of CsvAttributes.
   * @return Success(()) if the table was written, otherwise a Failure (up to 2.0.1, failures were ignored).
   */
  def writeCSVFileEncrypted[A: HexEncryption, Row](table: Table[Row])(path: Path, keyFunction: String => String)(using renderer: CsvRenderer[Row], generator: CsvGenerator[Row], hasKey: HasKey[Row], csvAttributes: CsvAttributes): Try[Unit] =
    CsvTableEncryptedFileRenderer[Row, A](path.toFile, keyFunction).render(table) map (_.close())
