package com.deweyvm.dogue.common.threading

import com.deweyvm.dogue.common.CommonImplicits
import CommonImplicits._

object DogueFuture {
  def createAndRun[T](f:() => T):DogueFuture[T] = {
    val task = new Task with DogueFuture[T] {
      private var result:Option[T] = None
      private var failed = false
      override def doWork() {
        val res = f().some
        setResult(res)
        kill()
      }
      override def exception(e:Exception) = synchronized {
        failed = true
      }
      private def setResult(r:Option[T]) = synchronized {
        result = r
      }
      override def getResult = synchronized { result }
      override def hasFailed = synchronized { failed }
    }
    ThreadManager.spawn(task)
  }
}

trait DogueFuture[T] {
  def getResult:Option[T]
  def hasFailed:Boolean
}
