package com.deweyvm.dogue.common.data

import com.deweyvm.gleany.data.Point2d

object Circle {
  def ccwPath(radius:Double, period:Double, t:Double):Point2d = {
    val a = t*Angles.Tau/period
    radius *: Point2d(math.sin(a), math.cos(a))
  }
  def cwPath(radius:Double, period:Double, t:Double):Point2d = {
    val a = t*Angles.Tau/period
    radius *: Point2d(math.sin(-a), math.cos(a))
  }
}

case class Circle(r:Double, center:Point2d) {
  def contains(pt:Point2d) = {
    (center - pt).magnitude2 < r*r
  }
}
