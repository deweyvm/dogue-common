package com.deweyvm.dogue.common.procgen

import com.deweyvm.gleany.data.Point2i
import scala.collection.mutable.ArrayBuffer
import com.deweyvm.dogue.common.Implicits
import Implicits._

case class Line(p1:Point2i, p2:Point2i) {
  def this(x1:Int, y1:Int, x2:Int, y2:Int) = this(Point2i(x1, y1), Point2i(x2, y2))
  def intersects(other:Line):Boolean = {
    false
  }
}
case class Polygon(points:Vector[Point2i]) {
  lazy val lines:Vector[Line] = {
    val toPair = points(points.length - 1) +: points
    ((0 until toPair.length - 1) map { i =>
      val p1 = toPair(i)
      val p2 = toPair(i+1)
      Line(p1, p2)
    }).toVector
  }
}

object PolygonUtils {

  def test() {
    val p1 = Point2i(0,0)
    val p2 = Point2i(-11,5)
    println(lineToPixels(Point2i(1,1), Point2i(-11,5)))
  }

  def pointInPolygon(poly:Polygon, pt:Point2i):Boolean = {
    val ray = new Line(pt, Point2i(Int.MaxValue, 0))
    val intersections = poly.lines.foldLeft(0){case (acc, line) =>
      if (ray.intersects(line)) {
        acc + 1
      } else {
        acc
      }
    }
    intersections.isOdd
  }

  private def lineToPixels(p1:Point2i, p2:Point2i):Vector[Point2i] = {
    import scala.math._
    val x1 = p1.x
    val y1 = p1.y
    val x2 = p2.x
    val y2 = p2.y
    val dX = abs(x2 - x1)
    val dY = abs(y2 - y1)
    var x = x1
    var y = y1

    val offsetX = if (x1 > x2) -1 else 1
    val offsetY = if (y1 > y2) -1 else 1
    val result = ArrayBuffer[Point2i]()
    result += Point2i(x,y)

    if (dX > dY) {
      var error = dX / 2

      while (x != x2) {
        error = error - dY
        if (error < 0) {
          y = y + offsetY
          error = error + dX
        }
        x = x + offsetX
        result += Point2i(x,y)
      }
    } else {
      var error = dY / 2
      while (y != y2) {
        error = error - dX
        if (error < 0) {
          x = x + offsetX
          error = error + dY
        }
        y = y + offsetY
        result += Point2i(x,y)
      }
    }
    result.toVector
  }
}
