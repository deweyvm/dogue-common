package com.deweyvm.dogue.common.procgen.voronoi

import com.deweyvm.gleany.data.Point2d

class VEdge(var start:Point2d, var left:Point2d, var right:Point2d) {

  var direction:Point2d = Point2d(right.y-left.y, -(right.x - left.x))
  var end:Point2d = null
  var f:Double = (right.x - left.x) / (left.y - right.y)
  var g:Double = start.y - f*start.x

  var neighbour:VEdge = null


}
