package com.deweyvm.dogue.common.procgen.voronoi

import com.deweyvm.dogue.common.procgen.{Node, Polygon, Graph}
import com.deweyvm.dogue.common.CommonImplicits
import CommonImplicits._
object VoronoiGraph {
  def makeGraph(faces:Vector[Polygon]):Graph[Polygon, Vector] = {
    val nodes : Map[Polygon, Node[Polygon, Vector]] = {
      faces.map { poly1 =>
        val neighbors = faces.map { poly2 =>
          if (poly1 != poly2 && poly1.isAdjacent(poly2)) {
            poly2.some
          } else {
            None
          }
        }.flatten
        val node = new Node[Polygon, Vector] {
          def self = poly1
          def getNeighbors = neighbors
          def isNeighbor(t:Polygon) = getNeighbors.contains(t)
        }
        (poly1, node)
      }.toMap
    }
    Graph.createVec(nodes)
  }
}

