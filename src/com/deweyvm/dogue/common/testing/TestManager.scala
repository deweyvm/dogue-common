package com.deweyvm.dogue.common.testing

import com.deweyvm.dogue.common.protocol.Command
import com.deweyvm.dogue.common.data.{EnrichedString, Array2d}
import com.deweyvm.dogue.common.logging.Log

object TestManager {
  private val tests = Vector[(() => Unit, String)](
    (Command.test, "Command"),
    (Array2d.test, "Array2d"),
    (EnrichedString.test, "EnrichedString")
  )
  def runAll(failFirst:Boolean) {
    tests map {case (f, n) => new Test {
      def runTests(): Unit = f()
      def name = n
    }} foreach { test =>
      test.run() match {
      case TestPass => println("[PASS] %15s" format test.name)
      case TestFail(msg) =>
        println("[FAIL] %15s" format test.name)
        if (failFirst) {
          throw new RuntimeException("Test failure: " + test.name)
        }
    }


    }
  }
}
