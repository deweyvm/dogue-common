package com.deweyvm.dogue.common.data

object Array2d {
  def tabulate[T](cols:Int, rows:Int)(f:(Int,Int) => T):Array2d[T] = {
    val elts = Vector.tabulate(cols*rows) { k =>
      val (i, j) = indexToCoords(k, cols)
      f(i, j)
    }
    new Array2d(elts, cols, rows)
  }

  /**
   * the parameter t is evaluated once
   */
  def fill[T](cols:Int, rows:Int)(t:T):Array2d[T] = {
    new Array2d(Vector.fill(cols*rows)(t), cols, rows)
  }

  def indexToCoords(k:Int, cols:Int):(Int,Int) = (k % cols, k / cols)
  def coordsToIndex(i:Int, j:Int, cols:Int):Int = i + j*cols

  def test() {
    val cols = 100
    val rows = 100
    val a = tabulate(rows, cols){ case (i, j) => scala.math.random}
    for (i <- 0 until rows; j <- 0 until cols) {
      val x = a.unsafeGet(i, j)
      val (ii, jj) = indexToCoords(coordsToIndex(i, j, cols), cols)
      val y = a.unsafeGet(ii, jj)
      assert(x == y)
    }
  }
}

class Array2d[+T](elements:Vector[T], cols:Int, rows:Int) {
  import Array2d._
  def foreach(f:(Int,Int,T) => Unit) {
    elements.zipWithIndex foreach { case (t, k) =>
      val (i, j) = indexToCoords(k, cols)
      f(i, j, t)
    }
  }

  private def unsafeGet(i:Int, j:Int):T = {
    val k = coordsToIndex(i, j, cols)
    elements(k)
  }

  def get(i:Int, j:Int):Option[T] = {
    val k = coordsToIndex(i, j, cols)
    if (k < 0 || k > cols*rows - 1)  {
      None
    } else {
      Some(elements(k))
    }
  }

  def put[R >: T](i:Int, j:Int, t:R):Array2d[R] =
    new Array2d(elements.updated(coordsToIndex(i, j, cols), t), cols, rows)


  def slice(x:Int, y:Int, width:Int, height:Int):Array2d[Option[T]] = {
    Array2d.tabulate(width, height) { case (i, j) =>
      get(i + x, j + y)
    }
  }
}
