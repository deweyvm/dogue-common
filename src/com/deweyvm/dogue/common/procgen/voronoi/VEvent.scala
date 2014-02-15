package com.deweyvm.dogue.common.procgen.voronoi

import com.deweyvm.gleany.data.Point2d
import scala.util.Random


case class VEvent(point:Point2d, isPlaceEvent:Boolean) {
  var y:Double = point.y
  var arch:VParabola = null
  var value:Int = 0
}
