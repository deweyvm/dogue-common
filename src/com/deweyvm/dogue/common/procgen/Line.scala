package com.deweyvm.dogue.common.procgen

import com.deweyvm.gleany.data.Point2d
import com.deweyvm.dogue.common.Implicits
import Implicits._
object Line {
  def test() {
    clockSignTest()
    adjacentTest()
    equalityTest()
  }

  private def adjacentTest() {
    assert(new Line(0,0,1,1).adjacentTo(Point2d(1,1)).isDefined)
    assert(new Line(0,0,1,1).adjacentTo(Point2d(0,0)).isDefined)
    assert(!new Line(0,0,1,1).adjacentTo(Point2d(1,2)).isDefined)
    assert(!new Line(0,0,1,1).adjacentTo(Point2d(2,1)).isDefined)
  }

  private def equalityTest() {
    assert(Line(Point2d.UnitX, Point2d.Zero) == Line(Point2d.Zero, Point2d.UnitX))
  }

  private def clockSignTest() {
    {
      val p = new Line(Point2d.Zero,Point2d.UnitX)
      for (i <- 0 until 90) {

        val angle = (i/90f)*3.141592

        val q = new Line(Point2d.UnitX,Point2d.UnitX + Point2d.UnitX.rotate(angle))
        assert(( q crossZ p) >= 0)
        val pq = p clockAngle q
        val qp = q clockAngle p
        assert(math.abs(pq - qp) < 0.001)
      }
    }
    {
      val p = new Line(Point2d.Zero,Point2d.UnitX)
      for (i <- 0 until 90) {

        val angle = (i/90f)*3.141592

        val q = new Line(Point2d.UnitX,Point2d.UnitX + Point2d.UnitX.rotate(-angle))
        assert(( q crossZ p) <= 0)
        val pq = p clockAngle q
        val qp = q clockAngle p
        assert(math.abs(pq - qp) < 0.001)
      }
    }
  }
}

case class Line(p:Point2d, q:Point2d) {
  def this(x1:Double, y1:Double, x2:Double, y2:Double) = this(Point2d(x1, y1), Point2d(x2, y2))
  def intersects(other:Line):Boolean = {
    false
  }
  def clockSign(other:Line):Int = {
    val p1 = p - q
    val p2 = -(other.p - other.q)
    math.signum((p1.to3 × p2.to3).z).toInt
  }

  def crossZ(other:Line):Double = {
    val p1 = p - q
    val p2 = -(other.p - other.q)
    (p1.to3 × p2.to3).z
  }

  def clockAngle(other:Line):Double = {
    val p1 = (p - q).normalize
    val p2 = (other.p - other.q).normalize
    math.acos(-p1 dot p2)
  }

  //Line must be reoriented such that the back is the start point and the front is the end point
  def adjacentTo(pt:Point2d):Option[Line] = {
    if (pt == p) {
      this.some
    } else if (pt == q) {
      Line(q,p).some
    } else {
      None
    }
  }

  override def equals(obj:Any) = {
    if (!obj.isInstanceOf[Line]) {
      false
    } else {
      val other = obj.asInstanceOf[Line]
      (other.p == p && other.q == q) || (other.q == p && other.p == q)

    }
  }


  override def toString = "<%.2f,%.2f --- %.2f,%.2f>" format (p.x, p.y, q.x, q.y)
}
