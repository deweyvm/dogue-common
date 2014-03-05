package com.deweyvm.dogue.common.data

import org.scalacheck.{Prop, Gen, Arbitrary}
import scala.util.Random
import com.deweyvm.dogue.common.testing.Test
import scala.Some
import com.deweyvm.dogue.common.Implicits
import Implicits._
import scala.collection.mutable

object Array2d {
  def tabulate[T](cols:Int, rows:Int)(f:(Int,Int) => T):Array2d[T] = {
    //println("tabulate")
    val array = new mutable.ArraySeq[T](cols*rows)
    for (k <- 0 until cols*rows) {
      val (i, j) = indexToCoords(k, cols)
      array(k) = f(i, j)
    }
    /*val elts = mutable.ArraySeq.tabulate(cols*rows) { k =>
      val (i, j) = indexToCoords(k, cols)
      f(i, j)
    }*/
    new Array2d(array, cols, rows)
  }

  def parTabulate[T](cols:Int, rows:Int)(f:(Int,Int) => T):Array2d[T] = {
    val elts = (0 until cols*rows).toVector.par.map { k =>
      val (i, j) = indexToCoords(k, cols)
      f(i, j)
    }
    new Array2d(mutable.ArraySeq(elts.toVector:_*), cols, rows)
  }

  /**
   * the parameter t is evaluated once
   */
  def fill[T](cols:Int, rows:Int)(t:T):Array2d[T] = {
    new Array2d(mutable.ArraySeq.fill(cols*rows)(t), cols, rows)
  }

  def unsafeGetElements[T](a:Array2d[T]):IndexedSeq[T] = a.elements

  @inline def indexToCoords(k:Int, cols:Int):(Int,Int) = (k % cols, k / cols)
  @inline def coordsToIndex(i:Int, j:Int, cols:Int):Int = i + j*cols

  def test() {
    implicit def arbA2d:Arbitrary[Array2d[Int]] =
      Arbitrary {
        for {
          rows <- Gen.choose(1,100)
          cols <- Gen.choose(1,100)
        } yield {
          tabulate(cols, rows){ case (i, j) =>
            Random.nextInt()
          }
        }
      }
    val prop = Prop.forAll { a:Array2d[Int] =>
      var fail = true
      a.foreach( {case (i, j, t) =>
        val x = a.unsafeGet(i, j)
        val (ii, jj) = indexToCoords(coordsToIndex(i, j, a.cols), a.cols)
        val y = a.unsafeGet(ii, jj)
        fail &&= x == y
      })
      fail

    }
    Test.runScalaCheck(prop, 1)
  }
}



class Array2d[T](private val elements:mutable.ArraySeq[T], cols_ :Int, rows_ :Int) {
  outer =>
  import Array2d._

  def rows = rows_
  def cols = cols_

  def strictGetAll:IndexedSeq[T] = elements

  def foreach(f:(Int,Int,T) => Unit) {
    elements.zipWithIndex foreach { case (t, k) =>
      val (i, j) = indexToCoords(k, cols)
      f(i, j, t)
    }
  }

  def groupBy[K](f:T => K) = elements.groupBy(f)

  def max[K >: T](implicit cmp: Ordering[K]):T = elements.max(cmp)
  def min[K >: T](implicit cmp: Ordering[K]):T = elements.min(cmp)

  def find(f:T=> Boolean):Option[(Int, Int, T)] = {
    elements.zipWithIndex.find {case (e, i) =>
      f(e)
    }.flatMap{case (e, i) =>
      val (x, y) = indexToCoords(i, cols)
      (x, y, e).some
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

  /*def slice(x:Int, y:Int, width:Int, height:Int, default:T):Array2dView[T] = new Array2dView[T] {
    val cols = width
    val rows = height
    def get(i:Int, j:Int):T = {
      outer.get(i + x, j + y).getOrElse(default)
    }
  }

  def sample(div:Int):Array2dView[T] = new Array2dView[T] {
    val cols = outer.cols/div
    val rows = outer.rows/div
    def get(i:Int, j:Int):T = {
      unsafeGet(i*div, j*div)
    }
  }*/

  def view:Array2dView[T] = new Array2dView[T] {
    val cols = outer.cols
    val rows = outer.rows
    def get(i:Int, j:Int):T = {
      unsafeGet(i, j)
    }
  }
}
