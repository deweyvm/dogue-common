package com.deweyvm.dogue.common.procgen

import com.deweyvm.gleany.data.Point2d
import com.deweyvm.dogue.common.Implicits
import Implicits._

object Polygon {
  def fromPoints(points:Vector[Point2d]):Polygon = {
    val toPair = points(points.length - 1) +: points
    val lines = (0 until toPair.length - 1).map { i =>
      val p1 = toPair(i)
      val p2 = toPair(i+1)
      Line(p1, p2)
    }
    Polygon(lines.toVector)
  }


  def test() {
    val poly = Polygon(Vector(
      new Line(1,1,3,2),
      new Line(3,2,4,1),
      new Line(4,1,6,4),
      new Line(6,4,7,1),
      new Line(7,1,7,7),
      new Line(7,7,4,3),
      new Line(4,3,5,6),
      new Line(5,6,3,4),
      new Line(3,4,2,7),
      new Line(2,7,1,6),
      new Line(1,6,1,1)


    ))
    val points = List(
      (Point2d(0,0), false),
      (Point2d(2,3), true),
      (Point2d(4,4), true),
      (Point2d(5,4), true),
      (Point2d(6,2), false),
      (Point2d(5,5), false),
      (Point2d(6,5), true),
      (Point2d(3,6), false),
      (Point2d(2,6), true)

    )

    points foreach { case (p, expect) =>
      val v = poly.contains(p)
      assert (v == expect, "%s expected (%s) got (%s)" format (p, expect, v))
    }

  }

}

case class Polygon(lines:Vector[Line]) {
  lazy val hash = lines.hashCode()
  //fixme issue #211
  lazy val points:Vector[Point2d] = {
    val pts = lines map {_.p}
    pts :+ pts(0)
  }
  lazy val signedArea:Double = {
    var sum = 0.0
    for (i <- 0 until points.length - 1) {
      val xi0 = points(i).x
      val xi1 = points(i+1).x
      val yi0 = points(i).y
      val yi1 = points(i+1).y
      sum +=  xi0*yi1 - xi1*yi0
    }
    sum/2
  }

  //fixme issue #212
  lazy val centroid:Point2d = {
    var cx = 0.0
    var cy = 0.0
    for (i <- 0 until points.length - 1) {
      val xi0 = points(i).x
      val xi1 = points(i+1).x
      val yi0 = points(i).y
      val yi1 = points(i+1).y
      val p = xi0*yi1 - xi1*yi0
      cx += (xi0 + xi1)*p
      cy += (yi0 + yi1)*p
    }
    Point2d(cx, cy)/(6*signedArea)
  }

  def upperLeft:Option[Point2d] = {
    val p1 = lines.map {l => Vector(l.p, l.q)}
    val points:Vector[Point2d] = p1.flatten

    points.sortBy(_.x).sortBy(_.y).headOption
  }

  def isAdjacent(other:Polygon):Boolean = {
    lines exists other.lines.contains
  }

  def contains(pt:Point2d):Boolean = {
    val ray = new Line(pt, Point2d(Int.MaxValue, 5000))
    val intersections = lines.foldLeft(0){case (acc, line) =>
      if (ray.intersectPoint(line).isDefined) {
        acc + 1
      } else {
        acc
      }
    }
    intersections.isOdd

  }

  def scale(s:Double) = {
    copy(lines = lines map {_.scale(s)})
  }

  override def equals(obj:Any) = {
    if (!obj.isInstanceOf[Polygon]) {
      false
    } else {
      val other = obj.asInstanceOf[Polygon]
      other.lines == lines
    }
  }

  override def hashCode() = hash
}
