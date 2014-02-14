package com.deweyvm.dogue.common.data

import org.scalacheck.{Prop, Gen, Arbitrary}
import scala.util.Random
import com.deweyvm.dogue.common.testing.Test
import scala.Some

object Array2d {
  def tabulate[T](cols:Int, rows:Int)(f:(Int,Int) => T):Array2d[T] = {
    val elts = Vector.tabulate(cols*rows) { k =>
      val (i, j) = indexToCoords(k, cols)
      f(i, j)
    }
    new Array2d(elts, cols, rows)
  }

  def parTabulate[T](cols:Int, rows:Int)(f:(Int,Int) => T):Array2d[T] = {
    val elts = (0 until cols*rows).toVector.par.map { k =>
      val (i, j) = indexToCoords(k, cols)
      f(i, j)
    }
    new Array2d(elts.toVector, cols, rows)
  }

  /**
   * the parameter t is evaluated once
   */
  def fill[T](cols:Int, rows:Int)(t:T):Array2d[T] = {
    new Array2d(Vector.fill(cols*rows)(t), cols, rows)
  }

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



class Array2d[+T](val elements:Vector[T], cols_ :Int, rows_ :Int) extends Indexed2d[T] {
  import Array2d._

  def rows = rows_
  def cols = cols_

  def strictGetAll:Vector[T] = elements

  def foreach(f:(Int,Int,T) => Unit) {
    elements.zipWithIndex foreach { case (t, k) =>
      val (i, j) = indexToCoords(k, cols)
      f(i, j, t)
    }
  }

  def groupBy[K](f:T=>K) = elements.groupBy(f)

  def max[K >: T](implicit cmp: Ordering[K]):T = elements.max(cmp)
  def min[K >: T](implicit cmp: Ordering[K]):T = elements.min(cmp)

  /**
   * Cut "this" down to the given size from the upper left corner.
   * If "this" is too small, fill in the extra slots with a default value.
   */
  def cut[K](c:Int, r:Int, f:T => K, default: => K):Array2d[K] = {
    Array2d.tabulate(c, r) { case (i, j) =>
      get(i, j).fold(default)(f)
    }
  }

  def map[K](f:(Int, Int, T) => K):Array2d[K] = {
    Array2d.tabulate(cols, rows) { case (i, j) =>
      val t = unsafeGet(i, j)
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


  def slice[K](x:Int, y:Int, width:Int, height:Int, f:T => K, default: => K):Array2d[K] = {
    Array2d.tabulate(width, height) { case (i, j) =>
      get(i + x, j + y).map(f).getOrElse(default)
    }
  }

  def sample(div:Int):Array2d[T] = {
    if (div <= 0) {
      throw new IllegalArgumentException("div must be > 0")
    }
    val (newCols, newRows) = (cols/div, rows/div)
    Array2d.tabulate(newCols, newRows) { case (i, j) =>
      unsafeGet(i*div, j*div)
    }
  }
}
