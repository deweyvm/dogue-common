package com.deweyvm.dogue.common.procgen.voronoi

import com.deweyvm.gleany.data.Point2d
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import scala.math

/**
 * credit to ivank
 */
class FortuneVoronoi(width:Int, height:Int, places:ArrayBuffer[Point2d]) {
  private val edges = ArrayBuffer[VEdge]()
  implicit val ordering = new Ordering[VEvent] {
    def compare(self:VEvent, other:VEvent):Int = {
      val b1:Boolean = self.y > other.y
      if (b1) 1 else -1
    }
  }
  private val queue = mutable.PriorityQueue[VEvent]()

  def getEdges:ArrayBuffer[VEdge] = {
    var root:VParabola = null
    queue.clear()
    for (i <- 0 until places.length) {
      val ev:VEvent = new VEvent(places(i), true)
      queue.enqueue(ev)
    }

    while(!queue.isEmpty) {
      val e = queue.dequeue()
      val lineY = e.point.y
      if(e.isPlaceEvent) {
        root = insertParabola(e.point, root, lineY)
      } else {
        removeParabola(e, root, lineY)
      }
    }
    finishEdge(root)
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

  private def insertParabola(p:Point2d, root:VParabola, lineY:Double):VParabola = {
    if(root == null) {
      return new VParabola(p)
    }
    val fp:Point2d = null
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
      return root
    }

    val par:VParabola = getParabolaByX(p.x, root, lineY)

    if(par.cEvent != null) {
      queueRemove(par.cEvent)
      par.cEvent = null
    }

    val start:Point2d = Point2d(p.x, getY(par.site, p.x, lineY))

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

    checkCircle(p0, lineY)
    checkCircle(p2, lineY)
    root
  }

  private def removeParabola(e:VEvent, root:VParabola, lineY:Double) {
    val p1:VParabola = e.arch

    val xl:VParabola = getLeftParent(p1)
    val xr:VParabola = getRightParent(p1)

    val p0:VParabola = getLeftChild(xl)
    val p2:VParabola = getRightChild(xr)

    if(p0.cEvent != null){
      queueRemove(p0.cEvent)
      p0.cEvent = null
    }
    if(p2.cEvent != null){
      queueRemove(p2.cEvent)
      p2.cEvent = null
    }

    val p:Point2d = Point2d(e.point.x, getY(p1.site, e.point.x, lineY))

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
    checkCircle(p0, lineY)
    checkCircle(p2, lineY)

  }

  private def finishEdge(n:VParabola) {
    val mx =
      if(n.edge.direction.x > 0.0) {
        math.max(width, n.edge.start.x + 10)
      } else {
        math.min(0.0, n.edge.start.x - 10)
      }
    n.edge.end = Point2d(mx, n.edge.f*mx + n.edge.g)

    if(!n.getLeft.isLeaf) {
      finishEdge(n.getLeft)
    }
    if(!n.getRight.isLeaf) {
      finishEdge(n.getRight)
    }
  }

  private def getXOfEdge(par:VParabola, y:Double):Double = {
    val left:VParabola = getLeftChild(par)
    val right:VParabola = getRightChild(par)

    val p:Point2d = left.site
    val r:Point2d = right.site

    val dp1:Double = 2*(p.y - y)
    val a1:Double = 1/dp1
    val b1:Double = -2*p.x/dp1
    val c1:Double = y+dp1/4 + p.x*p.x/dp1

    val dp2 = 2*(r.y - y)
    val a2:Double = 1/dp2
    val b2:Double = -2*r.x/dp2
    val c2:Double = y+dp2/4 + r.x*r.x/dp2

    val a:Double = a1 - a2
    val b:Double = b1 - b2
    val c:Double = c1 - c2

    val disc:Double = b*b - 4 * a * c
    val x1:Double = (-b + math.sqrt(disc)) / (2*a)
    val x2:Double = (-b - math.sqrt(disc)) / (2*a)

    if (p.y < r.y) {
      math.max(x1, x2)
    } else {
      math.min(x1, x2)
    }
  }

  private def getParabolaByX(xx:Double, root:VParabola, lineY:Double):VParabola = {
    var par:VParabola = root
    var x:Double = 0

    while(!par.isLeaf) {
      x = getXOfEdge(par, lineY)
      if(x > xx) {
        par = par.getLeft
      } else {
        par = par.getRight
      }
    }
    par
  }

  private def getY(p:Point2d, x:Double, lineY:Double) = {
    val dp = 2*(p.y - lineY)
    val b1 = -2*p.x/dp
    val c1 = lineY+dp/4 + p.x*p.x/dp

    x*x/dp + b1*x + c1
  }


  private def checkCircle(b:VParabola, lineY:Double) {
    val lp:VParabola = getLeftParent(b)
    val rp:VParabola = getRightParent(b)

    val a:VParabola = getLeftChild(lp)
    val c:VParabola = getRightChild(rp)

    if(a == null || c == null || a.site == c.site) {
      return
    }

    val s:Point2d = getEdgeIntersection(lp.edge, rp.edge)
    if(s == null) {
      return
    }

    val d:Double = (a.site - s).magnitude

    if(s.y - d  >= lineY) {
      return
    }

    val e:VEvent = new VEvent(new Point2d(s.x, s.y - d), false)

    b.cEvent = e
    e.arch = b
    queue.enqueue(e)
  }

  private def getEdgeIntersection(a:VEdge, b:VEdge):Point2d = {

    val x:Double = (b.g-a.g) / (a.f - b.f)
    val y:Double = a.f * x + a.g

    if (x.isNan()) {
      return null
    }
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

  private def getLeftParent(n:VParabola):VParabola = {
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

  private def getRightParent(n:VParabola):VParabola = {
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

  private def getLeftChild(n:VParabola):VParabola = {
    if(n == null) {
      return null
    }
    var par:VParabola = n.getLeft
    while(!par.isLeaf) {
      par = par.getRight
    }
    par
  }

  private def getRightChild(n:VParabola):VParabola = {
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
