package com.deweyvm.dogue.common.data

import com.deweyvm.gleany.data.Point2d

class Scalar[T](self:T)(implicit ev:Numeric[T]) {
  def dup:Point2d = {
    val d = ev.toDouble(self)
    Point2d(d, d)
  }
}
