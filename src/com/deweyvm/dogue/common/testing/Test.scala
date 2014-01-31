package com.deweyvm.dogue.common.testing

import com.deweyvm.dogue.common.logging.Log


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
