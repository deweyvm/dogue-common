package com.deweyvm.dogue.common.procgen.voronoi

import com.deweyvm.gleany.data.{Point3d, Point2f, Point2d}
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import com.deweyvm.dogue.common.Implicits
import Implicits._
import scala.math
import com.deweyvm.dogue.common.procgen.Line
import scala.util.control.Breaks

case class Edge(triStart:Point2d, triEnd:Point2d, vorStart:Point2d, vorEnd:Point2d) {
  def toLine = Line(vorStart, vorEnd)
}


object Voronoi {
  def getEdges(points:IndexedSeq[Point2d], width:Int, height:Int):Vector[Edge] = {
    val v = new FortuneVoronoi
    val buff = new ArrayBuffer[Point2d]()
    points foreach {buff += _}
    v.GetEdges(buff, width, height).map {edge =>
      Edge(edge.left, edge.right, edge.start, edge.end)
    }.toVector
  }
  /*
   */
  def getNext(lines:Vector[Line], line:Line, pt:Point2d, sign:Int):Option[(Line, Point2d)] = {
    val adjacent = lines.map {l => l adjacentTo pt}.flatten
    val touching = adjacent filter { l =>
      val sameLine = l == line
      val sameSign = (l clockSign line) == sign
      if (sameLine) {
        println("Ruled out %s same line" format l)
      }
      if (!sameSign) {
        println("Ruled out %s wrong sign" format l)
      }
      val result = !sameLine && sameSign
      if (result) {
        println("%s is acceptible" format l)
      }
      result
    }
    val sorted = touching sortBy { l =>
      println("    " + l)
      line clockAngle l
    }
    sorted match {
      case x +: xs =>
        if (pt == x.p) {
          println("endpoint is " + x.q)
          (x, x.q).some
        } else {
          println("endpoint is " + x.p)
          (x, x.p).some
        }
      case _ => None
    }

  }


  def followCircular(lines:Vector[Line], line:Line, sign:Int):Option[Vector[Line]] = {
    val currentPoly = ArrayBuffer[Line]()
    val startPoint = line.q
    val endPoint = line.p
    var currentLine = line
    var currentPoint = startPoint
    var broke = false
    Breaks.breakable {
      while(currentPoint != endPoint) {
        currentPoly += currentLine
        println("Current polygon:")
        currentPoly foreach {p =>
          println("    " + p)
        }
        println()
        println("next line   " + currentLine)
        println("endpoint is " + currentPoint)
        getNext(lines, currentLine, currentPoint, sign) match {
          case Some((tl, tp)) =>
            currentPoint = tp
            currentLine = tl

            if (currentPoly.length > lines.length) {
              Breaks.break()
            }
          case None =>
            println("~~~~~~~~~~BROKE~~~~~~~~~~")
            broke = true
            Breaks.break()
        }

      }
    }
    if (!broke && currentPoly.length >= 3) {
      currentPoly += currentLine
      println("~~~~~~~~~~ACCEPT~~~~~~~~~")

      currentPoly.toVector.some
    } else {
      None
    }
  }

  def getFaces(edges:Vector[Edge]):Vector[Vector[Line]] = {
    val lines = edges map {_.toLine}
    lines.foldLeft(Vector[Vector[Line]]()) { (acc, line) =>
       acc ++ followCircular(lines, line, -1) ++ followCircular(lines, line, 1)
    }
  }

  /**
   * keep a "store" listing how many polygons an edge is currently in.
   * every edge is in at most 2 polygons, so once the store hits two, that edge shuold never be used again.
   * for each edge, check in the store if it has been done twice.
   * if not, increment the store for tne current edge and walk it clockwise, adding each edge
   * you find to the polygon. if an edge doesnt have a proper partner, mark that edge as complete
   * (twice in the store). when finished, do the next edge which doesnt have 2 in the store.
   * @param edges
   */

  /*def naiveGetPolys(edges:IndexedSeq[Line]):Vector[Vector[Line]] = {
    val found = collection.mutable.Map[Line,Int]().withDefaultValue(0)

    /**
     * return the adjacent winded edge and the new outermost point
     *
     *             # <- this point (and its corresponding edge) is returned (if exists)
     *            #
     *           #
     *          #
     * ######### <- end is this point
     * @param end
     * @param sign clockwise or counterclockwise
     * @return
     */
    def findNextEdge(end:Point2d, edge:Line, sign:Int):Option[(Line, Point2d)] = {
      val touching: IndexedSeq[(Line, Point2d)] = edges.map { e =>
        if (e == edge) {
          None
        } else if (eqPoint(e.vorEnd, end)) {
          (e, e.vorEnd).some
        } else if (eqPoint(e.vorStart, end)) {
          (e, e.vorStart).some
        } else {
          None
        }

      }.flatten
      println("%d edges touching edge %s" format (touching.length, edge))
      val what = touching map { case (e,p) =>
        println(e)
        (edge, p, edge crossZ e, edge angle e)
      } filter { case (_,_,m,_) =>
        math.signum(m).toInt == sign
      } sortWith { case ((_,_,_,a), (_,_,_,b)) =>
        a < b
      }

      if (what.length > 0 && math.signum(what(0)._3).toInt != sign) {
        throw new Exception
      } else if (what.length > 0) {
        val f: (Line, Point2d, Double, Double) = what(0)
        println(" mag(%.2f) sign(%d) " format (f._3, sign))
        (f._1, f._2).some
      } else {
        None
      }

      /*val matching: IndexedSeq[Option[(Edge, Point2d)]] = edges.map {p =>
        val wind = edge.sign(p)
        if (eqPoint(p.vorEnd, end) && wind == sign) {
          (p, p.vorStart).some
        } else if (eqPoint(p.vorStart, end) && wind == sign) {
          (p, p.vorEnd).some
        } else {
          None
        }
      }
      val found: IndexedSeq[Option[(Edge, Point2d)]] = matching.filter{_.isDefined}
      found match {
        //case x +: y +: xs => throw new RuntimeException("more than one line with a the given turning")
        case x +: xs => x
        case Vector() => None
      }*/
    }
    import scala.util.control.Breaks._
    val result = ArrayBuffer[Vector[Line]]()
    def add(sign:Int) {
      edges foreach { edge =>
        val start = edge.vorStart
        val currentPoly = ArrayBuffer[Line]()
        currentPoly += edge
        var end: Point2d = edge.vorEnd
        var next = edge
        var broke = false
        breakable {
          while (end != start) {
            val n = findNextEdge(end, next, sign)
            n match {
              case Some((e, p)) =>
                end = p
                next = e
                println("added")
                currentPoly += e
              case None =>
                broke = true
                break()
            }
          }
        }
        found(edge) += 1
        if (currentPoly.length >= 3) {
          result += currentPoly.toVector
        }
      }

    }
    println("Adding")
    add(1)
    add(-1)
    result.toVector
  }*/
}

/**
 * credit to ivank
 */
class FortuneVoronoi {
  private var places:ArrayBuffer[Point2d] = null
  private var edges:ArrayBuffer[VEdge] = null
  implicit val ordering:Ordering[VEvent] = new Ordering[VEvent] {
    def compare(self:VEvent, other:VEvent):Int = {
      val b1:Boolean = self.y > other.y
      if (b1) 1 else -1
    }
  }
  private val queue:mutable.PriorityQueue[VEvent] = mutable.PriorityQueue[VEvent]()

  private var width:Int = 0
  private var height:Int = 0

  private var root:VParabola = null

  private var ly:Double = 0// line y
  private var lasty:Double = 0// last y

  private var fp:Point2d = null// first point


  def GetEdges(p:ArrayBuffer[Point2d], width:Int, height:Int):ArrayBuffer[VEdge] = {
    root = null
    this.places = p
    this.edges = ArrayBuffer[VEdge]()
    this.width = width
    this.height = height

    queue.clear()
    for (i <- 0 until places.length) {
      val ev:VEvent = new VEvent(places(i), true)
      queue.enqueue(ev)
    }

    var lasty:Double = Double.MaxValue

    while(!queue.isEmpty) {
      val e = queue.dequeue()
      ly = e.point.y
      if(e.pe) {
        InsertParabola(e.point)
      } else {
        RemoveParabola(e)
      }

      lasty = e.y
    }
    FinishEdge(root)
    for (i <- 0 until edges.length) {
      if(edges(i).neighbour != null) {
        edges(i).start = edges(i).neighbour.end
      }
    }

    edges
  }

  private def queueRemove(event:VEvent) {
    val toAdd = ArrayBuffer[VEvent]()
    var flag = true
    while (flag) {
      if (queue.isEmpty) {
        flag = false
        queue.enqueue(toAdd:_*)
      } else {
        val next = queue.dequeue()

        if (next == event) {
          queue.enqueue(toAdd:_*)
          flag = false
        } else {
          toAdd += next
        }
      }

    }
  }

  private def InsertParabola(p:Point2d) {
    if(root == null) {
      root = new VParabola(p)
      fp = p
      return
    }

    if (root.isLeaf && root.site.y - p.y < 1) {
      root.isLeaf = false
      root.setLeft(new VParabola(fp))
      root.setRight(new VParabola(p))
      val s:Point2d = Point2d((p.x+fp.x)/2, height)
      if (p.x > fp.x) {
        root.edge = new VEdge(s, fp, p)
      } else {
        root.edge = new VEdge(s, p, fp)
      }
      edges += root.edge
      return
    }

    val par:VParabola = GetParabolaByX(p.x)

    if(par.cEvent != null) {
      queueRemove(par.cEvent)
      par.cEvent = null
    }

    val start:Point2d = Point2d(p.x, GetY(par.site, p.x))

    val el:VEdge = new VEdge(start, par.site, p)
    val er:VEdge = new VEdge(start, p, par.site)

    el.neighbour = er
    edges += el

    par.edge = er
    par.isLeaf = false

    val p0:VParabola = new VParabola(par.site)
    val p1:VParabola = new VParabola(p)
    val p2:VParabola = new VParabola(par.site)

    par.setRight(p2)
    par.setLeft(new VParabola())
    par.getLeft.edge = el

    par.getLeft.setLeft(p0)
    par.getLeft.setRight(p1)

    CheckCircle(p0)
    CheckCircle(p2)
  }

  private def RemoveParabola(e:VEvent) {
    val p1:VParabola = e.arch

    val xl:VParabola = GetLeftParent(p1)
    val xr:VParabola = GetRightParent(p1)

    val p0:VParabola = GetLeftChild(xl)
    val p2:VParabola = GetRightChild(xr)

    if(p0.cEvent != null){
      queueRemove(p0.cEvent)
      p0.cEvent = null
    }
    if(p2.cEvent != null){
      queueRemove(p2.cEvent)
      p2.cEvent = null
    }

    val p:Point2d = Point2d(e.point.x, GetY(p1.site, e.point.x))

    lasty = e.point.y

    xl.edge.end = p
    xr.edge.end = p

    var higher:VParabola = null
    var par = p1
    while(par != root) {
      par = par.parent
      if(par == xl) {
        higher = xl
      }
      if(par == xr) {
        higher = xr
      }
    }

    higher.edge = new VEdge(p, p0.site, p2.site)

    edges += higher.edge

    val gparent:VParabola = p1.parent.parent
    if(p1.parent.getLeft == p1) {
      if(gparent.getLeft == p1.parent) {
        gparent.setLeft(p1.parent.getRight)
      } else {
        p1.parent.parent.setRight(p1.parent.getRight)
      }
    } else {
      if(gparent.getLeft == p1.parent) {
        gparent.setLeft(p1.parent.getLeft)
      } else {
        gparent.setRight(p1.parent.getLeft)
      }
    }
    CheckCircle(p0)
    CheckCircle(p2)
  }

  private def FinishEdge(n:VParabola) {
    val mx =
      if(n.edge.direction.x > 0.0) {
        math.max(width, n.edge.start.x + 10)
      } else {
        math.min(0.0, n.edge.start.x - 10)
      }
    n.edge.end = Point2d(mx, n.edge.f*mx + n.edge.g)

    if(!n.getLeft.isLeaf) {
      FinishEdge(n.getLeft)
    }
    if(!n.getRight.isLeaf) {
      FinishEdge(n.getRight)
    }
  }

  private def GetXOfEdge(par:VParabola, y:Double):Double = {
    val left:VParabola = GetLeftChild(par)
    val right:VParabola = GetRightChild(par)

    val p:Point2d = left.site
    val r:Point2d = right.site

    var dp:Double = 2*(p.y - y)
    val a1:Double = 1/dp
    val b1:Double = -2*p.x/dp
    val c1:Double = y+dp/4 + p.x*p.x/dp

    dp = 2*(r.y - y)
    val a2:Double = 1/dp
    val b2:Double = -2*r.x/dp
    val c2:Double = y+dp/4 + r.x*r.x/dp

    val a:Double = a1 - a2
    val b:Double = b1 - b2
    val c:Double = c1 - c2

    val disc:Double = b*b - 4 * a * c
    val x1:Double = (-b + math.sqrt(disc)) / (2*a)
    val x2:Double = (-b - math.sqrt(disc)) / (2*a)

    var ry:Double = 0
    if(p.y < r.y ) {
      ry =  math.max(x1, x2)
    } else {
      ry = math.min(x1, x2)
    }

    ry
  }

  def GetParabolaByX(xx:Double):VParabola = {
    var par:VParabola = root
    var x:Double = 0

    while(!par.isLeaf) {
      x = GetXOfEdge(par, ly)
      if(x > xx) {
        par = par.getLeft
      } else {
        par = par.getRight
      }
    }
    par
  }

  private def GetY(p:Point2d, x:Double) = {
    val dp = 2*(p.y - ly)
    val b1 = -2*p.x/dp
    val c1 = ly+dp/4 + p.x*p.x/dp

    x*x/dp + b1*x + c1
  }


  private def CheckCircle(b:VParabola) {
    val lp:VParabola = GetLeftParent(b)
    val rp:VParabola = GetRightParent(b)

    val a:VParabola = GetLeftChild(lp)
    val c:VParabola = GetRightChild(rp)

    if(a == null || c == null || a.site == c.site) {
      return
    }

    val s:Point2d = GetEdgeIntersection(lp.edge, rp.edge)
    if(s == null) {
      return
    }

    val d:Double = (a.site - s).magnitude

    if(s.y - d  >= ly) {
      return
    }

    val e:VEvent = new VEvent(new Point2d(s.x, s.y - d), false)

    b.cEvent = e
    e.arch = b
    queue.enqueue(e)
  }

  private def GetEdgeIntersection(a:VEdge, b:VEdge):Point2d = {

    val x:Double = (b.g-a.g) / (a.f - b.f)
    val y:Double = a.f * x + a.g

    if(math.abs(x) + math.abs(y) > 20*width) {
      return null
    } // parallel
    if(math.abs(a.direction.x) < 0.01 && math.abs(b.direction.x) < 0.01) {
      return null
    }

    if((x - a.start.x)/a.direction.x < 0) {
      return null
    }
    if((y - a.start.y)/a.direction.y < 0) {
      return null
    }

    if((x - b.start.x)/b.direction.x < 0) {
      return null
    }

    if((y - b.start.y)/b.direction.y < 0) {
      return null
    }

    Point2d(x, y)
  }


  /*private def GetLeft(n:VParabola):VParabola = {
    GetLeftChild(GetLeftParent(n))
  }

  private def GetRight(n:VParabola):VParabola = {
    GetRightChild(GetRightParent(n))
  }*/

  private def GetLeftParent(n:VParabola):VParabola = {
    var par:VParabola = n.parent
    var pLast:VParabola = n
    while(par.getLeft == pLast) {
      if(par.parent == null) {
        return null
      }
      pLast = par
      par = par.parent
    }
    par
  }

  private def GetRightParent(n:VParabola):VParabola = {
    var par:VParabola = n.parent
    var pLast:VParabola = n
    while(par.getRight == pLast) {
      if(par.parent == null) {
        return null
      }
      pLast = par
      par = par.parent
    }
    par
  }

  private def GetLeftChild(n:VParabola):VParabola = {
    if(n == null) {
      return null
    }
    var par:VParabola = n.getLeft
    while(!par.isLeaf) {
      par = par.getRight
    }
    par
  }

  private def GetRightChild(n:VParabola):VParabola = {
    if(n == null) {
      return null
    }
    var par:VParabola = n.getRight
    while(!par.isLeaf) {
      par = par.getLeft
    }
    par
  }


}
