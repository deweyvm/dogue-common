package com.deweyvm.dogue.common.procgen

import scala.util.Random
import com.deweyvm.dogue.common.data.Array2d
import com.deweyvm.gleany.data.Point2d
import com.deweyvm.dogue.common.Implicits
import Implicits._
class HexGrid(hexSize:Double, cols:Int, rows:Int, distortion:Double, seed:Long) {
  val hexCols = cols - 1
  val hexRows = (rows - 1)/2
  var debug = false
  def output(s:String) = if(debug) println(s)
  def makeHexes = {
    val factor = math.sqrt(3)/6
    val r = new Random(seed)
    def rd = (r.nextDouble() - 0.5)*distortion
    Array2d.tabulate(cols, rows) { case (i, j) =>
      val sign = (i.isOdd == j.isOdd).select(1, -1)
      hexSize *: Point2d(i*1.5 + sign*factor, j) + rd.dup
    }
  }

  def indexToCoords(i:Int) = {
    (i % hexCols, i / hexCols)
  }

  def coordsToIndex(i:Int, j:Int) = {
    i + j*hexCols
  }

  /**
   * must keep Option here so we can conveniently get adjacent hexes later
   */
  def makePolys:Vector[Option[Polygon]] = {
    val polys = for (i <- 0 until hexCols*hexRows) yield {
      val x0 = i % (cols - 1)
      val y0 = (i / (cols - 1)) * 2
      val yOffset = x0.isOdd select (1, 0)
      val x = x0
      val y = y0 + yOffset
      for {
        UL <- hexes.get(x,     y)
        UR <- hexes.get(x + 1, y)
        L  <- hexes.get(x,     y + 1)
        R  <- hexes.get(x + 1, y + 1)
        LL <- hexes.get(x,     y + 2)
        LR <- hexes.get(x + 1, y + 2)
      } yield {
        Polygon.fromPoints(Vector(UL, UR, R, LR, LL, L))
      }
    }
    polys.toVector
  }

  private def pop(s:String, opt:Option[Polygon]):Option[Polygon] = {
    opt match {
      case Some(_) => ()
        output(s + " occupied")
      case None => ()
        output(s + " vacant")
    }
    opt
  }

  def makeGraph:Graph[Polygon, Vector] = {
    val nodes_ :Vector[Node[Polygon, Vector]] = {
      val ns = for (i <- 0 until hexCols*hexRows) yield {
        val current:Option[Polygon] = polys(i)
        current map { center =>
          val (x, y) = indexToCoords(i)
          def getPoly(xOffset:Int, yOffset:Int):Option[Polygon] = {
            val k = coordsToIndex(x + xOffset, y + yOffset)
            if (k < 0 || k > polys.length - 1 || x + xOffset > hexCols - 1 || x + xOffset < 0) {
              None
            } else {
              polys(k)
            }
          }
          //println("Node " + i)
          val yOffset = x.isOdd select (0, -1)
          val UL = pop("Upper Left ", getPoly(-1,  0 + yOffset))
          val Up = pop("Upper      ", getPoly( 0, -1))
          val UR = pop("Upper Right", getPoly( 1,  0 + yOffset))
          val LL = pop("Lower Left ", getPoly(-1,  1 + yOffset))
          val Lo = pop("Lower      ", getPoly( 0,  1))
          val LR = pop("Lower Right", getPoly( 1,  1 + yOffset))
          val neighbors = Vector(UL, Up, UR, LL, Lo, LR).flatten
          val nSet = Set(neighbors:_*)
          new Node[Polygon, Vector]{
            def getNeighbors: Vector[Polygon] = neighbors
            def isNeighbor(t: Polygon): Boolean = nSet.contains(t)
            def self: Polygon = center
          }
        }


      }
      ns.toVector.flatten
    }

    new Graph[Polygon, Vector] {
      def nodes = nodes_
    }
  }

  val hexes = makeHexes
  val polys = makePolys
  val graph = makeGraph
}
