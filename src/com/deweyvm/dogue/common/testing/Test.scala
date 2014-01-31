package com.deweyvm.dogue.common.testing

import com.deweyvm.dogue.common.logging.Log
import org.scalacheck.Prop
import org.scalacheck.Gen.Parameters

object Test {
  def runScalaCheck(ps:Prop*) {
    ps foreach { p =>
      val result = p(Parameters.default)
      if (!result.success) {
        throw new RuntimeException(result.labels.headOption.getOrElse("Unknown") + " test failed")
      }
    }
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
