/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.render

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
    * Method to write a character sequence to the given instance o.
    *
    * @param o the instance of O whither the parameter x should be written.
    * @param x the character sequence to be written.
    * @return an instance of O which represents the updated output structure.
    */
  def writeRaw(o: O)(x: CharSequence): O

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
    * Elements will be separated by the delimite, but no newline is appended.
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
    * Method to write a value of type Any to the given instance o, possibly quoted.
    *
    * @param o the instance of O whither the parameter x should be written.
    * @param x the character sequence to be written.
    * @return an instance of O which represents the updated output structure.
    */
  def writeValue(o: O)(x: Any): O = if (x.toString.contains(delimiter.toString) || x.toString.contains(quote.toString))
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
  def writeQuoted(o: O)(x: CharSequence): O = writeRaw(o)(quote + x.toString.replaceAll(quote.toString, quote + quote.toString) + quote)

  /**
    * The default quote is one double-quote symbol
    *
    * @return "
    */
  def quote: CharSequence =
    """""""

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
