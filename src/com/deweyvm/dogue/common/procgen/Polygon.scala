package com.deweyvm.dogue.common.procgen

import com.deweyvm.gleany.data.Point2d
import com.deweyvm.dogue.common.Implicits
import Implicits._

object Polygon {
  def fromPoints(points:Vector[Point2d]):Polygon = {
    val toPair = points(points.length - 1) +: points
    val lines = ((0 until toPair.length - 1) map { i =>
      val p1 = toPair(i)
      val p2 = toPair(i+1)
      Line(p1, p2)
    }).toVector
    Polygon(lines)
  }
}

case class Polygon(lines:Vector[Line]) {
  lazy val set: Set[Line] = Set(lines:_*)

  def contains(pt:Point2d):Boolean = {
    val ray = new Line(pt, Point2d(Double.MaxValue, 0))
    val intersections = lines.foldLeft(0){case (acc, line) =>
      if (ray.intersectPoint(line).isDefined) {
        acc + 1
      } else {
        acc
      }
    }
    intersections.isOdd

  }

  override def equals(obj:Any) = {
    if (!obj.isInstanceOf[Polygon]) {
      false
    } else {
      val other = obj.asInstanceOf[Polygon]
      other.set == set
    }
  }

  override def hashCode() = set.hashCode()
}
