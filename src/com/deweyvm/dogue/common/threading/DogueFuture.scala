package com.deweyvm.dogue.common.threading

import com.deweyvm.dogue.common.CommonImplicits
import CommonImplicits._

object DogueFuture {
  def createAndRun[T](f:() => T):DogueFuture[T] = runProgress(f, () => 0.0, () => "")
  def runProgress[T](f:() => T, g:() => Double, d:() => String):ProgressFuture[T] = {
    val task = new Task with ProgressFuture[T] {
      private var result:Option[T] = None
      private var failed = false
      override def getProgress = g()
      override def getDescription = d()
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

trait ProgressFuture[T] extends DogueFuture[T] {
  def getProgress:Double
  def getDescription:String
}
