package com.deweyvm.dogue.common.procgen.voronoi

import com.deweyvm.gleany.data.Point2d
import com.deweyvm.dogue.common.procgen.Line

case class Edge(triStart:Point2d, triEnd:Point2d, vorStart:Point2d, vorEnd:Point2d) {
  def toLine = Line(vorStart, vorEnd)
}
