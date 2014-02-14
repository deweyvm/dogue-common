package com.deweyvm.dogue.common.procgen

import com.deweyvm.gleany.data.Point2d
import com.deweyvm.dogue.common.Implicits
import Implicits._

object VectorField {
  def simpleSpiral(width:Int, height:Int) = {
    def ddx(i:Double, j:Double):Double = {
      val x = i
      val y = j
      val mag = Point2d(x, y).magnitude
      y*mag
    }
    def ddy(i:Double, j:Double):Double = {
      val x = i
      val y = j
      val mag = Point2d(x, y).magnitude
      -x*mag
    }
    new VectorField(-width, -height, 2*width, 2*height, 40, ddx, ddy)
  }

  def windSpiral(width:Int, height:Int, scale:Int) = {
    val w = width/(scale*2)
    val h = height/(scale*2)

    def pow(k:Double) = math.pow(k, 1.5)

    def ddx(i:Double, j:Double):Double = {
      val p = Point2d(i, j)
      val mag = pow(w) - pow(p.magnitude)
      mag * p.normalize.y
    }
    def ddy(i:Double, j:Double):Double = {
      val p = Point2d(i, j)
      val mag = pow(w) - pow(p.magnitude)
      -mag * p.normalize.x
    }
    new VectorField(-width, -height, 2*width, 2*height, scale, ddx, ddy)
  }

  def reverseSpiral(width:Int, height:Int) = {
    def ddx(i:Double, j:Double):Double = {
      val x = i
      val y = j
      val mag = math.pow(Point2d(x, y).magnitude, 1.25)
      (y / mag)*40
    }
    def ddy(i:Double, j:Double):Double = {
      val x = i
      val y = j
      val mag = math.pow(Point2d(x, y).magnitude, 1.25)
      (-x / mag)*40
    }
    new VectorField(-width, -height, 2*width, 2*height, 40, ddx, ddy)
  }

}

class VectorField(x:Int, y:Int, width:Int, height:Int, div:Int, ddx:(Double, Double) => Double, ddy:(Double, Double) => Double) {
  val vectors = {
    for (i <- x/div until (x + width)/div;
         j <- y/div until (y + height)/div) yield {
      val x = ddx(i, j)
      val y = ddy(i, j)
      val v = Point2d(x, y)
      val mag = v.magnitude.clamp(10.0, 50.0)
      val d = v.normalize
      (Point2d(i*div, j*div), Arrow(d, mag))
    }
  }
}
