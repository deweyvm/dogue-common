package com.deweyvm.dogue.common.procgen

import scala.util.Random
import com.deweyvm.dogue.common.data.Array2d
import com.deweyvm.gleany.data.{Timer, Point2d}
import com.deweyvm.dogue.common.Implicits
import Implicits._
import scala.collection.immutable.IndexedSeq

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

object HexGrid {
  def test() {
    /*
     *   0       2
     *       1       3
     *   4       6
     *       5       7
     *   8       10
     *
     *
     * hexes 10 and 12 are partial and therefore arent full hexes. we do not test them
     */
    val hex = new HexGrid(32, 5, 7, 0, 0)
    val tests = List( //(polyNumber, numNeighbors)
      (0, 2),
      (1, 5),
      (2, 3),
      (3, 3),
      (4, 4),
      (5, 5),
      (6, 6),
      (7, 3),
      (8, 2),
      (10, 3)
    )
    (0 until 50) foreach { _ =>
      tests foreach { case (polyNumber, numNeighbors) =>
        val polyOpt = hex.polys(polyNumber)
        assert(polyOpt.isDefined, "poly %d undefined" format polyNumber)
        polyOpt foreach {poly =>
          val actualN = hex.graph.getNode(poly).map {_.getNeighbors.length}.getOrElse(-1)
          assert(actualN == numNeighbors, "Neighbors for %d: got (%d) expected (%d)" format (polyNumber, actualN, numNeighbors))
        }
      }
    }

  }
}

class HexGrid(val hexSize:Double, cols:Int, rows:Int, distortion:Double, seed:Long) {
  import Hex._
  val hexCols = cols - 1
  val hexRows = (rows - 1)/2
  private def makeHexes = {
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
  private def makePolys:Vector[Option[Polygon]] = {
    val polys = (0 until hexCols*hexRows).map{ i =>
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

  private def makeGraph:Graph[Polygon, Vector] = {
    val nodes:Map[Polygon, Node[Polygon, Vector]] = {
      val ns = polys.zipWithIndex.map { case (current, i) =>
        val (x, y) = indexToCoords(i, hexCols)
        current.map { center =>
          val node = new Node[Polygon, Vector]{
            lazy val neighbors = Hex.getNeighbors(x, y, polys, hexCols)
            lazy val nSet = Set(neighbors:_*)
            def getNeighbors: Vector[Polygon] = neighbors
            def isNeighbor(t: Polygon): Boolean = nSet.contains(t)
            def self: Polygon = center
          }
          (center, node)
        }
      }
      ns.flatten.toMap
    }
    Graph.createVec(nodes)
  }

  /**
   * Returns a list of polygons closest to the point for further checking
   */
  private def pointToPolys(px:Int, py:Int):Vector[Polygon] = {
    val x = (px/hexSize).toInt
    val yOffset = x.isOdd.select(-hexSize/2, 0)
    val y = ((3.0/4.0)*(py + yOffset)/hexSize).toInt
    val k = Hex.coordsToIndex(x, y, hexCols)
    if (k < polys.length && k >= 0) {
      val p = polys(k)
      p.map{_ +: Hex.getNeighbors(x, y, polys, hexCols)}.getOrElse(Vector())
    } else {
      Vector()
    }
  }

  /**
   * Returns a polygon that the given point is inside. If multiple polys match, an
   * arbitrary one is chosen and returned.
   */
  def pointInPoly(px:Int, py:Int):Option[Polygon] = {
    pointToPolys(px, py) find {_.contains(Point2d(px, py))}
  }

  val hexes = makeHexes
  val polys = makePolys
  val graph = makeGraph
}
