/*
 * Copyright (c) 2019. Phasmid Software
 */

package com.phasmidsoftware.tableparser.cats.parse

import cats.effect.IO
import com.phasmidsoftware.tableparser.core.parse.TableParser
import scala.annotation.implicitNotFound

/**
 * Type class to parse a set of rows as a Table.
 *
 * @tparam Table the Table type.
 */
@implicitNotFound(msg = "Cannot find an implicit instance of TableParser[${Table}]. Typically, you should define an instance of StringTableParser or StringsTableParser.")
trait TableParserIO[Table] extends TableParser[Table] {

  /**
   * Method to parse a table based on a sequence of Inputs.
   *
   * TODO this needs to use IO internally
   *
   * @param xs the sequence of Inputs, one for each row
   * @return an IO[Table]
   */
  def parseIO(xs: Iterator[Input], n: Int): IO[Table] = IO.fromTry(parse(xs, n))
}

