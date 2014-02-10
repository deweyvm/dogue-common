package com.deweyvm.dogue.common.data

import scala.collection.mutable.ArrayBuffer
import com.deweyvm.dogue.common.Implicits
import Implicits._
import scala.collection.mutable


class Lazy2d[T](getter:(Int, Int) => T, cols:Int, rows:Int) extends Indexed2d[T] {
  private val buffer = mutable.Map[Int,T]()
  private val set = mutable.Map[Int,Boolean]().withDefaultValue(false)

  def strictGetAll:Vector[T] = ((0 until cols*rows) map { i:Int =>
    val (x, y) = Array2d.indexToCoords(i, cols)
    unsafeGet(x, y)
  }).toVector

  override def get(i:Int, j:Int):Option[T] = {
    if (i < 0 || j < 0 || i > cols - 1 || j > rows - 1) {
      None
    } else {
      unsafeGet(i, j).some
    }
  }

  def foreach(f:(Int,Int,T) => Unit) {
    for (i <- 0 until cols; j <- 0 until rows) {
      f(i, j, unsafeGet(i, j))
    }
  }

  def map[K](f:(Int, Int, T) => K):Indexed2d[K] = {
    def newGetter(i:Int, j:Int):K = {
        val t = unsafeGet(i, j)
        f(i, j, t)
    }
    new Lazy2d(newGetter, cols, rows)
  }

  private def unsafeGet(i:Int, j:Int):T = {
    val index = Array2d.coordsToIndex(i, j, cols)
    if (set(index)) {
      buffer(index)
    } else {
      val res = getter(i, j)
      buffer(index) = res
      set(index) = true
      res
    }
  }

  def cut[K](c:Int, r:Int, f:T => K, default: => K):Indexed2d[K] = {
    def newGetter(i:Int, j:Int):K = {
      get(i, j).fold(default)(f)
    }
    new Lazy2d(newGetter, c, r)
  }

  def slice(x:Int, y:Int, width:Int, height:Int):Indexed2d[Option[T]] = {
    def newGetter(i:Int, j:Int):Option[T] = {
      get(i + x, j + y)
    }
    new Lazy2d(newGetter, width, height)
  }
}
