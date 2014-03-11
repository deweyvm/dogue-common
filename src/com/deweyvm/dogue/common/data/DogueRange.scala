package com.deweyvm.dogue.common.data

class RangeOp[T](a:T) {
  def <=>(other:T) = new DogueRange(a, other)
}

class DogueRange[T](val min:T, val max:T) {
  def contains(t:T)(implicit o:Ordering[T]) = {
    o.gteq(t, min) && o.lteq(t, max)
  }
}
