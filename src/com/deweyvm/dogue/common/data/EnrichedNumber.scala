package com.deweyvm.dogue.common.data

class EnrichedNumber[T](rep:T)(implicit n: Numeric[T]) {
  def clamp(min: T, max: T):T = {
    if (n.lt(rep, min)) {
      min
    } else if (n.gt(rep, max)) {
      max
    } else {
      rep
    }
  }
}

