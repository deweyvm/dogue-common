package com.deweyvm.dogue.common.testing

import com.deweyvm.dogue.common.data.{LockedQueue, EnrichedString, Array2d}
import com.deweyvm.dogue.common.parsing.CommandParser

object TestManager {
  private val tests = Vector[(() => Unit, String)](
    (CommandParser.test, "CommandParser"),
    (Array2d.test, "Array2d"),
    (EnrichedString.test, "EnrichedString"),
    (LockedQueue.test, "LockedQueue")
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
          throw new RuntimeException("Test failure: " + test.name + "\n" + msg)
        }
    }


    }
  }
}
