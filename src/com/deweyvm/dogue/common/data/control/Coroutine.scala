package com.deweyvm.dogue.common.data.control

import com.deweyvm.gleany.data.Timer


trait Coroutine[T] {
  type Result
  def apply():Result
  /**
   * Warning: calling this function will cause the next part of the
   * coroutine to execute if it has not already.
   * @return number of nanoseconds it took to run the apply method
   */
  def nanos:Long
}
case class Return[T](f:() => T) extends Coroutine[T] {
  lazy val (t, result) = Timer.timer(f)
  override type Result = T
  def apply(): T = result

  /** @inheritdoc */
  def nanos = t
}

case class Yield[T](progress:Int, label:String, f:()=>Coroutine[T]) extends Coroutine[T] {
  override type Result = Coroutine[T]
  lazy val (t, result) = Timer.timer(f)
  def apply(): Coroutine[T] = result

  /** @inheritdoc */
  def nanos = t
}
