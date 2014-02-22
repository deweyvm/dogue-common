package com.deweyvm.dogue.common.data

import com.deweyvm.gleany.data.Point2d

case class Circle(r:Double, center:Point2d) {
  def contains(pt:Point2d) = {
    (center - pt).magnitude2 < r*r
  }
}
