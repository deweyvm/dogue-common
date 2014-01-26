package com.deweyvm.dogue.common.threading


object ThreadManager {
  def spawn[T <: Task](task:T): T = {
    new Thread(new Runnable {
      override def run() {
        task.execute()
      }
    }).start()
    task
  }
}
