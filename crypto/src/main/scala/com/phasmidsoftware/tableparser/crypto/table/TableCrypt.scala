/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.crypto.table

import cats.effect.{IO, Resource}
import com.phasmidsoftware.tableparser.crypto.HexEncryption
import com.phasmidsoftware.tableparser.core.render._
import com.phasmidsoftware.tableparser.core.table.{CsvAttributes, HasKey, Table}
import java.nio.file.{Files, Path}
import scala.language.postfixOps
import scala.util.Try

object TableCrypt:

  /**
   * Method to write a Table[Row] as a CSV file, with the header in plain text and each row encrypted
   * (as EncryptedHeadedStringTableParser reads it): each row is its key, "|", and the encrypted row (as Hex).
   * The rows are encrypted (in parallel) and the file written when the IO is run;
   * the file is closed whether or not the IO succeeds.
   *
   * @param table       the table.
   * @param path        the Path where the output should be stored.
   * @param keyFunction a function which takes the key of a row (see HasKey) and returns the encryption key for that row
   *                    (for AES128CTR, 16 characters).
   * @param renderer    implicit value of CsvRenderer[Row].
   * @param generator   implicit value of CsvGenerator[Row] (for the header).
   * @param hasKey      implicit value of HasKey[Row].
   *                    This relates to a column which is the "key" column in a CSV (used for identification).
   *                    It is not directly related to cryptography.
   * @tparam A the cipher algorithm (for which there must be evidence of HexEncryption[A]).
   * @param csvAttributes implicit value of CsvAttributes (its newline ends each line).
   * @return an IO[Unit].
   */
  def writeCSVFileEncryptedIO[A: HexEncryption, Row](table: Table[Row])(path: Path, keyFunction: String => String)(using renderer: CsvRenderer[Row], generator: CsvGenerator[Row], hasKey: HasKey[Row], csvAttributes: CsvAttributes): IO[Unit] =
    val header: String = generator match
      case g: CsvProductGenerator[?] => g.toColumnNames(None, None)
      case g => g.toColumnName(None, "")

    def encryptRow(row: Row): IO[String] =
      val key = hasKey.key(row)
      for
        rawKey <- IO(keyFunction(key))
        hex <- summon[HexEncryption[A]].encryptHex(rawKey, renderer.render(row, Map()))
      yield s"$key|$hex"

    for
      lines <- IO.parTraverseN(4)(table.toSeq)(encryptRow)
      _ <- Resource.fromAutoCloseable(IO.blocking(Files.newBufferedWriter(path))).use: w =>
        IO.blocking((header +: lines).foreach(line => w.write(s"$line${csvAttributes.newline}")))
    yield ()

  /**
   * Method to write a Table[Row] as a CSV file, with the header in plain text and each row encrypted:
   * this is writeCSVFileEncryptedIO, run synchronously.
   *
   * @param table       the table.
   * @param path        the Path where the output should be stored.
   * @param keyFunction a function which takes the key of a row (see HasKey) and returns the encryption key for that row.
   * @tparam A the cipher algorithm (for which there must be evidence of HexEncryption[A]).
   * @return Success(()) if the table was written, otherwise a Failure (up to 2.0.1, failures were ignored).
   */
  def writeCSVFileEncrypted[A: HexEncryption, Row](table: Table[Row])(path: Path, keyFunction: String => String)(using renderer: CsvRenderer[Row], generator: CsvGenerator[Row], hasKey: HasKey[Row], csvAttributes: CsvAttributes): Try[Unit] =
    import cats.effect.unsafe.implicits.global
    // NOTE this is the one place where writing runs the IO (for the sake of a synchronous, Try-based method).
    writeCSVFileEncryptedIO[A, Row](table)(path, keyFunction).attempt.unsafeRunSync().toTry
