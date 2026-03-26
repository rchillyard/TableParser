/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.cats.table

import com.phasmidsoftware.tableparser.cats.crypto.HexEncryption
import com.phasmidsoftware.tableparser.cats.render.CsvTableEncryptedFileRenderer
import com.phasmidsoftware.tableparser.core.render._
import com.phasmidsoftware.tableparser.core.table.{CsvAttributes, HasKey, Table}
import java.io.File
import scala.language.postfixOps

object TableCrypt {

  /**
   * Method to render this Table[T] as a CSV file with (maybe) header.
   *
   * @param file      instance of File where the output should be stored.
   * @param renderer  implicit value of CsvRenderer[Row].
   * @param generator implicit value of CsvProductGenerator[Row].
   * @param hasKey    implicit value of HasKey[Row].
   *                  This relates to a column which is the "key" column in a CSV (used for identification).
   *                  It is not directly related to cryptography.
   * @tparam A the cipher algorithm (for which there must be evidence of HexEncryption[A]).
   * @param csvAttributes implicit value of CsvAttributes.
   */
  @deprecated("Use writeCSVFileEncrypted(Path) instead", "1.3.0")
  def writeCSVFileEncrypted[A: HexEncryption, Row](table: Table[Row])(file: File)(implicit renderer: CsvRenderer[Row], generator: CsvGenerator[Row], hasKey: HasKey[Row], csvAttributes: CsvAttributes): Unit =
    CsvTableEncryptedFileRenderer[Row, A](file).render(table)
}
