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

  protected def init() {

  }

  //used to do additional kill notifications when this object's kill method is called
  protected def killAux() {

  }

  protected def doWork():Unit

  protected def cleanup() {

  }

  protected def exception(t:Exception) {

  }

  def isRunning = running

  private def logException(t:Exception) {
    Log.warn(Log.formatStackTrace(t))
  }

  final def execute() {
    init()
    try {
      while (running && isRunning) {
        doWork()
      }
    } catch {
      case t:Exception =>
        logException(t)
        exception(t)
    } finally {
      cleanup()
    }
  }

  final def start() = {
    ThreadManager.spawn(this)
  }
}
