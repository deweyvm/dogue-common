package com.deweyvm.dogue.common.procgen

import scala.util.Random
import com.deweyvm.dogue.common.data.Array2d
import com.deweyvm.gleany.data.{Point2i, Point2d}
import com.deweyvm.dogue.common.Implicits
import Implicits._

object Hex {
  case object Up extends HexDirection {
    def getOffset(i:Int) = (0, -1)
  }
  case object Down extends HexDirection {
    def getOffset(i:Int) = (0, 1)
  }
  case object UpRight extends HexDirection {
    def getOffset(i:Int) = (1, getStutter(i))
  }
  case object DownRight extends HexDirection {
    def getOffset(i:Int) = (1, 1 + getStutter(i))
  }
  case object UpLeft extends HexDirection {
    def getOffset(i:Int) = (-1, getStutter(i))
  }
  case object DownLeft extends HexDirection {
    def getOffset(i:Int) = (-1, 1 + getStutter(i))
  }

  def getAll = Vector(Up, Down, UpRight, DownRight, UpLeft, DownLeft)

  def indexToCoords(i:Int, hexCols:Int) = {
    (i % hexCols, i / hexCols)
  }

  def coordsToIndex(i:Int, j:Int, hexCols:Int) = {
    i + j*hexCols
  }

  def getNeighborOffsets(i:Int): Vector[(Int, Int)] = {
    getAll map { _.getOffset(i) }
  }

  def getNeighbors(x:Int, y:Int, polys:Vector[Option[Polygon]], hexCols:Int):Vector[Polygon] = {
    def getPoly(offset:(Int,Int)):Option[Polygon] = {
      val (iOffset, jOffset) = offset
      val k = coordsToIndex(x + iOffset, y + jOffset, hexCols)
      if (k < 0 || k > polys.length - 1 || x + iOffset > hexCols - 1 || x + iOffset < 0) {
        None
      } else {
        polys(k)
      }
    }

    getNeighborOffsets(x).map(getPoly).flatten
  }
}

trait HexDirection {
  protected def getStutter(i:Int) = i.isOdd select (0, -1)
  def getOffset(i:Int):(Int, Int)
  def getCoords(iBase:Int, jBase:Int):(Int, Int) = {
    val (io, jo) = getOffset(iBase)
    (iBase + io, jBase + jo)
  }
}

class HexGrid(val hexSize:Double, cols:Int, rows:Int, distortion:Double, seed:Long) {
  import Hex._
  val hexCols = cols - 1
  val hexRows = (rows - 1)/2
  var debug = false
  def output(s:String) = if (debug) println(s)
  def makeHexes = {
    val factor = math.sqrt(3)/8
    val r = new Random(seed)
    def rd = (r.nextDouble() - 0.5)*distortion
    Array2d.tabulate(cols, rows) { case (i, j) =>
      val sign = (i.isOdd == j.isOdd).select(1, -1)
      hexSize *: Point2d(i*1 + sign*factor, j*(2/3.0)) + rd.dup
    }
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

  def makeGraph:Graph[Polygon, Vector] = {
    val nodes_ :Vector[Node[Polygon, Vector]] = {
      val ns = for (i <- 0 until hexCols*hexRows) yield {
        val current:Option[Polygon] = polys(i)
        current map { center =>
          val (x, y) = indexToCoords(i, hexCols)
          val neighbors = Hex.getNeighbors(x, y, polys, hexCols)
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

  def pointToPoly(x:Int, y:Int):Option[Polygon] = {

  }

  val hexes = makeHexes
  val polys = makePolys
  val graph = makeGraph
}
