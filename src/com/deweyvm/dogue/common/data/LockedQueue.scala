package com.deweyvm.dogue.common.data

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Arbitrary}
import com.deweyvm.dogue.common.testing.Test
import com.deweyvm.dogue.common.Implicits
import Implicits._

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

    val p3 = forAll { q:LockedQueue[String] =>
      val size = q.length
      val threads = (0 until 100) map { _ =>
        new Thread {
          override def run(): Unit = {
            q.dequeueIfNotEmpty match {
              case Some(s) => q.enqueue(s)
              case None => ()
            }

          }
        }
      }
      threads foreach {_.start()}
      threads foreach {_.join()}
      val result = size == q.length
      if (!result) {
        Test.outputln(q.length - size)
      }
      result

    }.label("Queue threading test")

    Test.runScalaCheck(p1, 1)
    Test.runScalaCheck(p2, 1)
    Test.runScalaCheck(p3, 1)

  }
}

class LockedQueue[A] {
  private val queue = collection.mutable.Queue[A]()

  def length = synchronized { queue.length }

  def enqueue(s:A) {
    synchronized { queue.enqueue(s) }
  }

  def enqueueAll(as:Vector[A]) {
    synchronized { queue.enqueue(as:_*) }
  }

  def dequeue():A = synchronized {
    synchronized { queue.dequeue() }
  }

  def dequeueIfNotEmpty:Option[A] = synchronized {
    if (!queue.isEmpty) {
      queue.dequeue().some
    } else {
      None
    }

  }

  def isEmpty:Boolean = synchronized {
    queue.isEmpty
  }

  def dequeueAll():Vector[A] = synchronized {
    queue.dequeueAll(_ => true).toVector
  }

}
