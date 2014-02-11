package com.deweyvm.dogue.common.procgen

import com.deweyvm.gleany.data.Point2i
import scala.collection.mutable.ArrayBuffer
import scala.collection.script.End

class Polygon(points:Vector[Point2i])

object PolygonUtils {

  def test() {
    val p1 = Point2i(0,0)
    val p2 = Point2i(-11,5)
    println(allOcts(Point2i(1,1), Point2i(-11,5)))
  }

  def lineToPixels(p1:Point2i, p2:Point2i) = {
    val (less, more) =
      if (p1.x < p2.x) {
        (p1, p2)
      } else {
        (p2, p1)
      }

    val ySign = if (more.y - less.y < 0) -1 else 1
    blineToPixels(less, more) map {p => p.copy(y=ySign*p.y)}

  }

  def pointInPolygon(poly:Polygon, pt:Point2i)

  private def allOcts(p1:Point2i, p2:Point2i):Vector[Point2i] = {
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

  private def blineToPixels(p1:Point2i, p2:Point2i):Vector[Point2i] = {
    val dx = p2.x - p1.x
    val dy = p2.y - p1.y
    if (dx == 0) {
      return (0 until dy).map {y => p1 + Point2i(0, y)}.toVector
    }
    val result = ArrayBuffer[Point2i]()
    var error = 0.0
    var deltaerr = scala.math.abs(dy/dx.toDouble)
    var y = p1.y
    (p1.x until p2.x) foreach { x =>
      result += Point2i(x, y)
      error += deltaerr
      if (error > 0.5) {
        y += 1
        error = error - 1
      }
    }
    result.toVector
  }
}
