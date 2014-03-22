package com.deweyvm.dogue.common.data

import com.deweyvm.dogue.common.CommonImplicits._

object Pointer {
  def create[T](elts:Vector[T], reqPtr:Int):Pointer[T] = {
    val ptr = reqPtr.clamp(0, elts.length - 1)
    new Pointer(elts, ptr)
  }

  def create[T](elt:T, elts:T*):Pointer[T] = {
    new Pointer(elt +: elts.toVector, 0)
  }

}

class Pointer[T] private (elts:Vector[T], ptr:Int) {
  def toSeq = elts.toSeq
  def get:T = elts(ptr)
  def getMap[K](f:T => T):Pointer[T] = {
    new Pointer[T](elts.updated(ptr, f(get)), ptr)
  }

  def map[K](f:T => K):Pointer[K] = {
    new Pointer[K](elts map f, ptr)
  }

  def selectMap[K](selected:T => K, notSelected:T => K): Seq[K] = elts.zipWithIndex map { case (t, i) =>
    if (ptr == i) {
      selected(t)
    } else {
      notSelected(t)
    }
  }
  def zipWithIndex:Pointer[(T, Int)] = new Pointer(elts.zipWithIndex, ptr)
  def length:Int = elts.length
  def updated(offset:Int):Pointer[T] = {
    new Pointer(elts, (ptr + offset + length) % length)
  }
}
