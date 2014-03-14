package com.deweyvm.dogue.common.data.serialization

trait Readable[T] {
  def read:Option[T]
}
