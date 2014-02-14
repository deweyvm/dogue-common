package com.deweyvm.dogue.common.procgen

import com.deweyvm.gleany.data.Point2d

case class Arrow(direction:Point2d, magnitude:Double) {
  val headSize = 10

  def getHead(line:Line):(Line, Line, Line) = {
    val s1 = line.q
    val e1 = s1 + (line.q - line.p).rotate(3 * 3.1415/4).normalize * headSize
    val s2 = line.q
    val e2 = s2 + (line.q - line.p).rotate(3 * -3.1415/4).normalize * headSize
    val s3 = e1
    val e3 = e2
    (Line(s1, e1), Line(s2, e2), Line(s3, e3))
  }
  def getShapes(pos:Point2d):(Line, (Line, Line, Line)) = {
    val start = pos
    val end = pos + magnitude *: direction
    val l = Line(start, end)
    (l, getHead(l))
  }

}
