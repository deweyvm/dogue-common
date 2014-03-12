package com.deweyvm.dogue.common.procgen

import com.deweyvm.gleany.data.{Point2d, Point2i}
import scala.collection.mutable.ArrayBuffer


object PolygonUtils {


  def flattenVector(pts:Vector[Point2d]):Array[Float] = {
    val flat = pts.foldRight(Vector[Float]()){ case (p, acc) =>
      p.x.toFloat +: (p.y.toFloat +: acc)
    }
    Array(flat:_*)
  }

  def lineToPixels(p1:Point2i, p2:Point2i):Vector[Point2i] = {
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
