package com.deweyvm.dogue.common.data

import com.deweyvm.dogue.common.threading.Lock
import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Arbitrary}
import com.deweyvm.dogue.common.testing.Test

object LockedQueue {
  implicit def arbQ[T](implicit a:Arbitrary[T]):Arbitrary[LockedQueue[T]] =
    Arbitrary {
      val items = Gen.listOf(a.arbitrary)
      val result = new LockedQueue[T]
      for (l <- items) yield {
        result.enqueueAll(l.toVector)
        result
      }
    }



  def test() {
    val p1 = forAll { q:LockedQueue[Int] =>
      q.dequeueAll()
      q.isEmpty
    }.label("Empty queue")

    val p2 = forAll { (q:LockedQueue[Int], r:LockedQueue[Int]) =>
      val len = q.length + r.length
      q.enqueueAll(r.dequeueAll())
      q.length == len
    }.label("Queue sum")

    Test.runScalaCheck(p1, p2)

  }
}

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
    lock.map(f)(queue)
  }

}
