package com.deweyvm.dogue.common.data.algebra

import java.util.logging.Logger

trait Monoid[A] {
  def +(a1:A, a2:A):A
  def zero:A
}

object Monoid {
  implicit def VectorMonoid[A]:Monoid[Vector[A]] = new Monoid[Vector[A]] {
    def +(a1:Vector[A], a2:Vector[A]) = a1 ++ a2
    def zero = Vector()
  }
}

