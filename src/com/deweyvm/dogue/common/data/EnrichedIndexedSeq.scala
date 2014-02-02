package com.deweyvm.dogue.common.data

import com.deweyvm.dogue.common.Implicits._

class EnrichedIndexedSeq[T](self:IndexedSeq[T]) {
  def tryGet(i:Int):Option[T] =
    if (i > self.length - 1 || i < 0) {
      None
    } else {
      self(i).some
    }
}
