package com.phasmidsoftware.table

trait Validity[T] {

  def isValid(t: T): Boolean
}
