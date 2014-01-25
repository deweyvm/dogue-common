package com.deweyvm.dogue.common.threading

import com.deweyvm.dogue.common.threading.Task

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
