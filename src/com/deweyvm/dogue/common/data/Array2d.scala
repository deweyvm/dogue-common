package com.deweyvm.dogue.common.data

import org.scalacheck.{Prop, Gen, Arbitrary}
import scala.util.Random
import com.deweyvm.dogue.common.testing.Test
import scala.Some
import com.deweyvm.dogue.common.CommonImplicits
import CommonImplicits._
import scala.collection.mutable

object Array2d {
  def tabulate[T](cols:Int, rows:Int)(f:(Int,Int) => T):Array2d[T] = {
    val array = new mutable.ArraySeq[T](cols*rows)
    for (k <- 0 until cols*rows) {
      val (i, j) = indexToCoords(k, cols)
      array(k) = f(i, j)
    }
    new Array2d(array, cols, rows)
  }

  def parTabulate[T](cols:Int, rows:Int)(f:(Int,Int) => T):Array2d[T] = {
    val elts = (0 until cols*rows).par.map { k =>
      val (i, j) = indexToCoords(k, cols)
      f(i, j)
    }
    new Array2d(mutable.ArraySeq(elts.seq:_*), cols, rows)
  }

  /**
   * the parameter t is evaluated once
   */
  def fill[T](cols:Int, rows:Int)(t:T):Array2d[T] = {
    new Array2d(mutable.ArraySeq.fill(cols*rows)(t), cols, rows)
  }

  def unsafeGetElements[T](a:Array2d[T]):IndexedSeq[T] = a.elements

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
    val prop = Prop.forAll { a:Array2d[Int] =>
      var fail = true
      a.foreach( {case (i, j, t) =>
        val x = a.get(i, j)
        val (ii, jj) = indexToCoords(coordsToIndex(i, j, a.cols), a.cols)
        val y = a.get(ii, jj)
        fail &&= x == y
      })
      fail

    }
    Test.runScalaCheck(prop, 1)
  }
}



class Array2d[T](private val elements:mutable.ArraySeq[T],
                 override val cols :Int,
                 override val rows :Int) extends Array2dView[T] {
  outer =>
  import Array2d._

  def strictGetAll:IndexedSeq[T] = elements


  def map[K](f:(Int,Int,T)=>K) = {
    Array2d.tabulate(cols, rows) { case (i, j) =>
      val elt = get(i, j)
      f(i, j, elt)
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

  def get(i:Int, j:Int):T = {
    val k = coordsToIndex(i, j, cols)
    elements(k)
  }

  def getOption(i:Int, j:Int):Option[T] = {
    if (i < 0 || j < 0 || i > cols - 1 || j > rows - 1) {
      None
    } else {
      get(i, j).some
    }
  }
}
