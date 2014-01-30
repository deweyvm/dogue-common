package com.deweyvm.dogue.common.threading

class Lock {
  val lock = new scala.concurrent.Lock()
  def map[A,B](f:A => B): A => B = { case a =>
    lock.acquire()
    val result = f(a)
    lock.release()
    result
  }

  def get[T](f:() => T):T = {
    lock.acquire()
    val result = f()
    lock.release()
    result
  }
}
