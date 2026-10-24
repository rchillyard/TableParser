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

object TableCrypt {

  /**
   * Method to render this Table[T] as a CSV file with (maybe) header.
   *
   * @param path      the Path where the output should be stored.
   * @param renderer  implicit value of CsvRenderer[Row].
   * @param generator implicit value of CsvProductGenerator[Row].
   * @param hasKey    implicit value of HasKey[Row].
   *                  This relates to a column which is the "key" column in a CSV (used for identification).
   *                  It is not directly related to cryptography.
   * @tparam A the cipher algorithm (for which there must be evidence of HexEncryption[A]).
   * @param csvAttributes implicit value of CsvAttributes.
   */
  def writeCSVFileEncrypted[A: HexEncryption, Row](table: Table[Row])(path: Path)(using renderer: CsvRenderer[Row], generator: CsvGenerator[Row], hasKey: HasKey[Row], csvAttributes: CsvAttributes): Unit =
    CsvTableEncryptedFileRenderer[Row, A](path.toFile).render(table)
}
