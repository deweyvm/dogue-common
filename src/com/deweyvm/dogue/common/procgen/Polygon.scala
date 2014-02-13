package com.deweyvm.dogue.common.procgen

import com.deweyvm.gleany.data.Point2d

case class Polygon(points:Vector[Point2d]) {
  lazy val lines:Vector[Line] = {
    val toPair = points(points.length - 1) +: points
    ((0 until toPair.length - 1) map { i =>
      val p1 = toPair(i)
      val p2 = toPair(i+1)
      Line(p1, p2)
    }).toVector
  }
}
