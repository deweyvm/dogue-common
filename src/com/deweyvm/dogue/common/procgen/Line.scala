package com.deweyvm.dogue.common.procgen

case class Line(p:Point2d, q:Point2d) {
  def this(x1:Int, y1:Int, x2:Int, y2:Int) = this(Point2d(x1, y1), Point2d(x2, y2))
  def intersects(other:Line):Boolean = {
    false
  }
  def sign(other:Line):Int = {
    val p1 = p - q
    val p2 = other.p - other.q
    math.signum((p1.to3 × p2.to3).z).toInt
  }

  def crossZ(other:Line):Double = {
    val p1 = p - q
    val p2 = other.p - other.q
    (p1.to3 × p2.to3).z
  }

  def angle(other:Line):Double = {
    val p1 = (p - q).normalize
    val p2 = (other.p - other.q).normalize
    math.acos(p1 dot p2)
  }

  def adjacentTo(other:Point2d) = p == other || q == other

  override def toString = "<%.2f,%.2f --- %.2f,%.2f>" format (p.x, p.y, q.x, q.y)
}
