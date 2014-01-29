package com.deweyvm.dogue.common.data

import scala.concurrent.Lock

class LockedQueue[A] {
  private val lock = new Lock
  private val queue = collection.mutable.Queue[A]()

  def length = lockedDo(_.length)

  def enqueue(s:A) {
    lockedDo(_.enqueue(s))
  }

  def enqueueAll(as:Vector[A]) {
    lockedDo(_.enqueue(as:_*))
  }

  def dequeue():A = {
    lockedDo(_.dequeue())
  }

  def isEmpty:Boolean = {
    lockedDo(_.isEmpty)
  }

  def dequeueAll():Vector[A] = {
    lockedDo(_.dequeueAll(_ => true).toVector)
  }

  private def lockedDo[T](f:collection.mutable.Queue[A] => T):T = {
    lock.acquire()
    val result = f(queue)
    lock.release()
    result
  }

}
