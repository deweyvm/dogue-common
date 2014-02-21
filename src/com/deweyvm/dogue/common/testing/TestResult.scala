package com.deweyvm.dogue.common.testing

trait TestResult {
  def isFailure:Boolean = this match {
    case TestPass => false
    case _ => true
  }
}
case object TestPass extends TestResult
case class TestFail(msg:String) extends TestResult
