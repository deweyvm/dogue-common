package com.deweyvm.dogue.common.threading

import com.deweyvm.dogue.common.logging.Log


abstract class Task {

  private var running = true

  final def kill() {
    if (running) {
      running = false
      killAux()
    }
  }

  //used to do additional kill notifications when this object's kill method is called
  def killAux() {

  }

  def doWork():Unit
  def cleanup():Unit
  def isRunning = running

  def logException(t:Throwable) {
    Log.warn(Log.formatStackTrace(t))
  }

  final def execute() {
    try {
      while (running && isRunning) {
        doWork()
      }
    } catch {
      case t:Throwable => logException(t)
    } finally {
      cleanup()
    }
  }

  final def start():Task = {
    ThreadManager.spawn(this)
    this
  }
}
