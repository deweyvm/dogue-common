package com.deweyvm.dogue.common.testing

import com.deweyvm.dogue.common.data.{Array2d, EnrichedString, LockedQueue}
import com.deweyvm.dogue.common.parsing.CommandParser
import com.deweyvm.dogue.common.procgen._

object TestManager {
  private val tests = Vector[(() => Unit, String)](
    (CommandParser.test, "CommandParser"),
    (Array2d.test, "Array2d"),
    (EnrichedString.test, "EnrichedString"),
    (LockedQueue.test, "LockedQueue"),
    (Line.test, "Line"),
    (Polygon.test, "Polygon"),
    (VectorField.test, "VectorField"),
    (HexGrid.test, "HexGrid")
  )

  def runAll(failFirst:Boolean) {
    tests map {case (f, n) => new Test {
      def runTests(): Unit = f()
      def name = n
    }} map { test =>
      val result = test.run()
      printResult(result, test.name)
      result match {
        case TestFail(msg) if failFirst =>
          throw new RuntimeException("Test failure: " + test.name + "\n" + msg)
        case _ => ()
      }
    }
  }

  private def printResult(result:TestResult, name:String) = result match {
    case TestPass =>
      Test.outputln("[PASS] %15s" format name)
    case TestFail(msg) =>
      Test.outputln("[FAIL] %15s" format name)
  }
}
