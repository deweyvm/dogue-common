package com.deweyvm.dogue.common.data

import com.deweyvm.dogue.common.Implicits._

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
  def get:T = elts(ptr)
  def map(f:T=>T) = new Pointer(elts.updated(ptr, f(get)), ptr)
  def length:Int = elts.length
  def updated(offset:Int):Pointer[T] = {
    new Pointer(elts, (ptr + offset + length) % length)
  }
}
