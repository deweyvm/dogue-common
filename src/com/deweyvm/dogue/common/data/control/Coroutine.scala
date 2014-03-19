package com.deweyvm.dogue.common.data.control

import com.deweyvm.gleany.data.Timer


trait Coroutine[T]
case class Return[T](f:() => T) extends Coroutine[T]


object Yield {
  def unapply[T](y:Yield[T]):Option[(Int, String, () => Coroutine[T])] = {
    Some((y.progress, y.label, y.getResult))
  }

  def apply[T](progress:Int, label:String, f:() => Coroutine[T]) = new Yield(progress, label, f)
}
class Yield[T](val progress:Int, val label:String, private val f:()=>Coroutine[T]) extends Coroutine[T] {
  private def getResult = { () => {
      val (t, result) = Timer.timer(f)
      YieldResult(t, result)
    }
  }
}

case class YieldResult[T](time:Long, next:Coroutine[T]) extends Coroutine[T]
