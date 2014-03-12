package com.deweyvm.dogue.common.data

import com.deweyvm.dogue.common.CommonImplicits._
import scala.util.Random

class EnrichedIndexedSeq[T](self:IndexedSeq[T]) {
  def tryGet(i:Int):Option[T] =
    if (i > self.length - 1 || i < 0) {
      None
    } else {
      self(i).some
    }


  def getRandom:T = self(Random.nextInt(self.length))

  def getRandom(r:Random):T = self(r.nextInt(self.length))


  def pickN(n:Int) = {
    Random.shuffle(self).take(n)
  }

}
