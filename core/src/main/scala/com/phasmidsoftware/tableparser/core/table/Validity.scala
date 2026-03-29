package com.phasmidsoftware.tableparser.core.table

trait Validity[T] {

  def isValid(t: T): Boolean
}
