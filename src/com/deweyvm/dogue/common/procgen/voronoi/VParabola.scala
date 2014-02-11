package com.deweyvm.dogue.common.procgen.voronoi

import com.deweyvm.gleany.data.Point2d

class VParabola(var site:Point2d = null) {
  var cEvent:VEvent = null

  var parent:VParabola = null
  private var left:VParabola = null
  private var right:VParabola = null
  var isLeaf:Boolean = site != null

  var edge:VEdge = null

  def setLeft(p:VParabola) {
    left = p
    p.parent = this
  }

  def setRight(p:VParabola) {
    right = p
    p.parent = this
  }

  def getLeft:VParabola = left
  def getRight:VParabola = right

}
