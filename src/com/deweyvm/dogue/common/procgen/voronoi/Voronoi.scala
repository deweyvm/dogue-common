package com.deweyvm.dogue.common.procgen.voronoi

import com.deweyvm.dogue.common.procgen.{Polygon, Line}
import com.deweyvm.gleany.data.{Recti, Rectd, Point2d}
import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks
import com.deweyvm.dogue.common.Implicits
import Implicits._
import scala.util.Random

object Voronoi {
  val debugPrint = false
  def debug(s:String) = {
    if (debugPrint) {
      System.out.print(s + "\n")
    }
  }

  type LineMap = Map[Line,Int]


  def check(rect:Rectd, edges:Vector[Edge]):Option[Vector[Edge]] = {
    var intersections = 0
    edges foreach { e1 =>
      edges foreach { e2 =>
        val l1 = e1.toLine
        val l2 = e2.toLine
        val intersect = l1.intersectPoint(l2)
        intersect.map { pt =>
          if (rect.contains(pt) &&
            l1.getAdjacentEpsilon(pt, 0.01).isEmpty &&
            l2.getAdjacentEpsilon(pt, 0.01).isEmpty) {
            intersections += 1
          }
        }
      }
    }
    if (intersections > 0) {
      None
    } else {
      edges.some
    }
  }

  def getEdges(points:IndexedSeq[Point2d], width:Double, height:Double, seed:Long):Vector[Edge] = {
    val r = new Random(seed)
    var nudgeAmount = 0.5
    def nudge = (r.nextDouble() - 0.5)*nudgeAmount
    var myPoints = points
    var count = 0
    while (true) {
      val v = new FortuneVoronoi(width, height, myPoints)
      val result = v.getEdges.map {edge =>
        Edge(edge.left, edge.right, edge.start, edge.end)
      }.toVector

      check(Rectd(0,0,width, height), result) match {
        case Some(edges) =>
          return edges
        case None =>
          println("nudging " + count)
          count += 1
          nudgeAmount *= 2
          myPoints = myPoints map { pt =>
            pt + Point2d(nudge, nudge)
          }
      }
    }
    return Vector()//dummy, impossible
  }

  def getNext(lines:Vector[Line], line:Line, pt:Point2d, sign:Int):Option[(Line, Point2d)] = {
    val adjacent = lines.map {l => l getAdjacent pt}.flatten
    val touching = adjacent filter { l =>
      val sameLine = l == line
      val sameSign = (l clockSign line) == sign
      if (sameLine) {
        debug("Ruled out %s same line" format l)
      }
      if (!sameSign) {
        debug("Ruled out %s wrong sign" format l)
      }
      val result = !sameLine && sameSign
      if (result) {
        debug("%s is acceptible" format l)
      }
      result
    }
    val sorted = touching sortBy { l =>
      debug("    " + l)
      line clockAngle l
    }
    sorted match {
      case x +: xs =>
        if (pt == x.p) {
          debug("endpoint is " + x.q)
          (x, x.q).some
        } else {
          debug("endpoint is " + x.p)
          (x, x.p).some
        }
      case _ => None
    }
  }

  def followCircular(set:LineMap, lines:Vector[Line], line:Line, sign:Int, rect:Rectd):(LineMap, Option[Polygon]) = {
    var newSet = set
    val currentPoly = ArrayBuffer[Line]()
    val endPoint = line.p
    var currentLine = line
    var currentPoint = line.q
    var broke = false
    Breaks.breakable {
      while(currentPoint != endPoint) {
        currentPoly += currentLine
        if (newSet(line) == 2) {
          return (newSet, None)
        }
        newSet = newSet.updated(currentLine, newSet(currentLine) + 1)

        debug("Current polygon:") ; currentPoly foreach {p => debug("    " + p) } ; debug("") ; debug("next line   " + currentLine) ; debug("endpoint is " + currentPoint)

        getNext(lines, currentLine, currentPoint, sign) match {
          case Some((tl, tp)) =>
            currentPoint = tp
            currentLine = tl
            if (currentPoly.length > lines.length) {
              Breaks.break()
            }
          case None =>
            debug("~~~~~~~~~~BROKE~~~~~~~~~~")
            broke = true
            Breaks.break()
        }

      }
    }
    if (!broke && currentPoly.length >= 3) {
      currentPoly += currentLine
      if (currentPoly.exists{l => !rect.contains(l.p) || !rect.contains(l.q)}) {
        (newSet, None)
      } else {
        if (newSet(line) > 2) {
          (newSet, None)
        } else {
          debug("~~~~~~~~~~ACCEPT~~~~~~~~~")
          (newSet, Polygon(currentPoly.toSet).some)
        }

      }
    } else {
      (newSet, None)
    }
  }

  def getFaces(edges:Vector[Edge], rect:Rectd):Vector[Polygon] = {
    val lines = edges map { _.toLine }
    val startMap = Map[Line,Int]().withDefaultValue(0)
    val faces = lines.foldLeft((Vector[Polygon](), startMap)) { case ((polys, set), line) =>
      val (s1, ccw) = followCircular(set, lines, line, -1, rect)
      val (s2, cw) = followCircular(s1, lines, line, 1, rect)
      (polys ++ ccw ++ cw, s2)
    }

    val result = Set(faces._1:_*).toVector
    result
  }
}
