package com.deweyvm.dogue.common.procgen.voronoi

import com.deweyvm.dogue.common.procgen.{Node, Polygon, Graph}
import com.deweyvm.dogue.common.Implicits
import Implicits._

/*class VoronoiGraph(faces:Vector[Polygon]) extends Graph[Polygon, Vector] {
  private val nodes_ = {

    faces map { poly1 =>
      val neighbors = faces.map { poly2 =>
        if (poly1 != poly2 && poly1.isAdjacent(poly2)) {
          poly2.some
        } else {
          None
        }
      }.flatten
      new Node[Polygon, Vector] {
        def self = poly1
        def getNeighbors = neighbors
        def isNeighbor(t:Polygon) = getNeighbors.contains(t)
      }
    }
  }


  def nodes = nodes_
}*/
