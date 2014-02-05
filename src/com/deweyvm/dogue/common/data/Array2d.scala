package com.deweyvm.dogue.common.data

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import scala.util.Random
import com.deweyvm.dogue.common.testing.Test

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
    val prop = forAll { a:Array2d[Int] =>
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

class Array2d[+T](elements:Vector[T], val cols:Int, val rows:Int) {
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
