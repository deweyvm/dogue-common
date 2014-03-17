package com.deweyvm.dogue.common.data

import scala.collection.mutable.ArrayBuffer

trait Array2dView[T] {
  outer =>
  val cols:Int
  val rows:Int
  def get(i:Int, j:Int):T
  def foreach(f:(Int,Int,T) => Unit):Unit = {
    for (i <- 0 until cols; j <- 0 until rows) {
      f(i, j, get(i, j))
    }
  }

  def slice(x:Int, y:Int, width:Int, height:Int):Array2dView[T] = new Array2dView[T] {
    val cols = width
    val rows = height
    def get(i:Int, j:Int):T = {
      outer.get(i + x, j + y)
    }
  }

  def sample(div:Int):Array2dView[T] = new Array2dView[T] {
    val cols = outer.cols/div
    val rows = outer.rows/div
    def get(i:Int, j:Int):T = {
      outer.get(i*div, j*div)
    }
  }

  def viewMap[K](f:(Int, Int, T) => K):Array2dView[K] = new Array2dView[K] {
    val cols = outer.cols
    val rows = outer.rows
    def get(i:Int, j:Int):K = {
      f(i, j, outer.get(i, j))
    }
  }

  def toVector:Vector[T] = {
    val buff = ArrayBuffer[T]()
    for (i <- 0 until cols; j <- 0 until rows) yield {
      buff += get(i, j)
    }
    buff.toVector
  }
}
