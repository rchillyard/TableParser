/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

import com.phasmidsoftware.crypto.Encryption

import java.io.{File, FileWriter}

/**
 * Trait to enable rendering of a table to a sequential (non-hierarchical) output format.
 *
 * @tparam O the underlying type, for example, a StringBuilder.
 */
trait Writable[O] {

  // TODO create some off-the-shelf Writables

  /**
   * Method to return an empty (i.e. new) instance of O
   *
   * @return
   */
  def unit: O

  /**
   * Method to close this Writable, if appropriate.
   */
  def close(o: O): Unit = ()

  /**
   * Method to write a character sequence to the given instance o.
   *
   * @param o the instance of O whither the parameter x should be written.
   * @param x the character sequence to be written.
   * @return an instance of O which represents the updated output structure.
   */
  def writeRaw(o: O)(x: CharSequence): O

  /**
   * Method to write a character sequence to the given instance o followed by a newline.
   *
   * @param o the instance of O whither the parameter x should be written.
   * @param x the character sequence to be written.
   * @return an instance of O which represents the updated output structure.
   */
  def writeRawLine(o: O)(x: CharSequence): O = writeRaw(writeRaw(o)(x))(newline)

  /**
   * Method to write a value of type Any to the given instance o, possibly quoted.
   *
   * @param o the instance of O whither the xs values should be written.
   * @param x the row instance to be written.
   * @return an instance of O which represents the updated output structure.
   */
  def writeRow[Row <: Product](o: O)(x: Row): O = writeRaw(writeRowElements(o)(x.productIterator.toSeq))(newline)

  /**
   * Method to write a value of type Any to the given instance o, possibly quoted.
   * Elements will be separated by the delimiter, but no newline is appended.
   * Element strings may be enclosed in quotes if appropriate.
   *
   * @param o  the instance of O whither the xs values should be written.
   * @param xs the sequence of elements (values) to be written.
   * @return an instance of O which represents the updated output structure.
   */
  def writeRowElements(o: O)(xs: Seq[Any]): O = {
    // CONSIDER using foldLeft so that we can use the updated value of o at each step.
    for (x <- xs.headOption) writeValue(o)(x)
    for (x <- xs.tail) writeValue(writeRaw(o)(delimiter))(x)
    o
  }

  /**
   * Method to write a character sequence to the given instance o followed by a newline.
   *
   * @param o          the instance of O whither the parameter x should be written.
   * @param key        the lookup key for this row (not an encryption key).
   * @param plaintext  the plain text character sequence to be written in encrypted form.
   * @param encryption (implicit) instance of Encryption[A].
   * @return an instance of O which represents the updated output structure.
   */
  def writeLineEncrypted[A](o: O)(key: String, plaintext: CharSequence)(implicit encryption: Encryption[A]): O = {
    import cats.effect.unsafe.implicits.global

    val wBi = for {
      rawKey <- encryption.genRawKey
      cipherKey <- encryption.buildKey(rawKey)
      cipherText <- encryption.encrypt(cipherKey)(plaintext.toString)
      bytes <- encryption.concat(cipherText)
      hex <- Encryption.bytesToHexString(bytes)
      ok <- encryption.checkHex(hex, cipherKey, plaintext.toString)
    } yield (rawKey, hex, ok)

    // TODO use IO for O
    wBi.unsafeRunSync() match {
      case (k, w, true) =>
        println(s"$key: $k") // CONSIDER writing these key/password pairs to the log file.
        writeRaw(writeRaw(o)(s"$key|$w"))(newline)
      case _ => throw new RuntimeException(s"Writable.writeLineEncrypted: logic error: wBi=$wBi, plaintext=$plaintext")
    }
  }

  private val sQuote: String = quote.toString

  /**
   * Method to write a value of type Any to the given instance o, possibly quoted.
   *
   * @param o the instance of O whither the parameter x should be written.
   * @param x the character sequence to be written.
   * @return an instance of O which represents the updated output structure.
   */
  def writeValue(o: O)(x: Any): O = if (x.toString.contains(delimiter.toString) || x.toString.contains(sQuote))
    writeQuoted(o)(x.toString)
  else
    writeRaw(o)(x.toString)

  /**
   * Method to write a character sequence to the given instance o, but within quotes.
   * Any quote characters in x will be doubled.
   *
   * @param o the instance of O whither the parameter x should be written.
   * @param x the character sequence to be written.
   * @return an instance of O which represents the updated output structure.
   */
  def writeQuoted(o: O)(x: CharSequence): O = {
    val w = x.toString.replaceAll(quote.toString, s"$quote$quote")
    writeRaw(o)(s"$quote$w$quote")
  }

  /**
   * The default quote is one double-quote symbol
   *
   * @return "
   */
  def quote: CharSequence = """""""

  /**
   * The default delimiter is a comma followed by a space.
   *
   * @return ", "
   */
  def delimiter: CharSequence = ", "

  /**
   * The default newline character is the newline.
   *
   * @return \n
   */
  def newline: CharSequence = "\n"
}

object Writable {
  def stringBuilderWritable(delim: CharSequence = ",", quoteChar: CharSequence = """""""): Writable[StringBuilder] = new Writable[StringBuilder] {
    def writeRaw(o: StringBuilder)(x: CharSequence): StringBuilder = o.append(x)

    def unit: StringBuilder = new StringBuilder

    override def delimiter: CharSequence = delim

    override def quote: CharSequence = quoteChar
  }

  def fileWritable(file: File): Writable[FileWriter] = new Writable[FileWriter] {
    def unit: FileWriter = new FileWriter(file)

    def writeRaw(o: FileWriter)(x: CharSequence): FileWriter = o.append(x).asInstanceOf[FileWriter]

    override def close(o: FileWriter): Unit = o.close()
  }
}