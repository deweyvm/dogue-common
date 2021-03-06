package com.deweyvm.dogue.common.threading

import com.deweyvm.dogue.common.CommonImplicits
import CommonImplicits._

object DogueFuture {
  def createAndRun[T](f:() => T):DogueFuture[T] = {
    val task = new Task with DogueFuture[T] {
      private var result:Option[T] = None
      private var exc:Option[Exception] = None
      override def doWork() {
        val res = f().some
        setResult(res)
        kill()
      }
      override def exception(e:Exception) = synchronized {
        exc = e.some
      }
      private def setResult(r:Option[T]) = synchronized {
        result = r
      }
      override def getResult = synchronized { result }
      override def getFailure = synchronized { exc }
    }
    ThreadManager.spawn(task)
  }
}

trait DogueFuture[T] {
  def getResult:Option[T]
  def getFailure:Option[Exception]
}
