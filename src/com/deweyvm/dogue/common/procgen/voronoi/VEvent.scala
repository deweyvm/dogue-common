package com.deweyvm.dogue.common.procgen.voronoi

import com.deweyvm.gleany.data.Point2d
import scala.util.Random


case class VEvent(point:Point2d, pe:Boolean) {


  var y:Double = point.y
  var key:Int = Random.nextInt(1000000000)

  var arch:VParabola = null

  var value:Int = 0



}
