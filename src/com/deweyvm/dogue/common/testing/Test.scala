package com.deweyvm.dogue.common.testing

import com.deweyvm.dogue.common.logging.Log
import org.scalacheck.Prop
import org.scalacheck.Gen.Parameters
import org.scalacheck.Test.Parameters.Default
import org.scalacheck.util.ConsoleReporter
import org.scalacheck.Test.Result

object Test {
  def runScalaCheck(p:Prop, workers:Int) {
    p.check(new Default{}.
      withWorkers(workers).
      withTestCallback(new ConsoleReporter(1) {
      override def onTestResult(name: String, result: Result) {
        if (!result.passed) {
          super.onTestResult(name, result)
          throw new RuntimeException("test failed")
        } else {

        }
      }
    }))
  }
}

trait Test {
  def name:String
  def runTests():Unit

  final def run():TestResult =  {
    try {
      runTests()
      TestPass
    } catch {
      case t:Throwable =>
        val msg = Log.formatStackTrace(t)
        TestFail(msg)
    }
  }

}
