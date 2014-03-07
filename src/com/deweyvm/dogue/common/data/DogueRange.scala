package com.deweyvm.dogue.common.data

class RangeCompanion[T](a:T) {
  def <=>(other:T) = new DogueRange(a, other)
}

class DogueRange[T](min:T, max:T) {
  def contains(t:T)(implicit o:Ordering[T]) = {
    o.gteq(t, min) && o.lt(t, max)
  }
}
