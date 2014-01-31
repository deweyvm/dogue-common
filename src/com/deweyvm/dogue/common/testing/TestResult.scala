package com.deweyvm.dogue.common.testing

trait TestResult
case object TestPass extends TestResult
case class TestFail(msg:String) extends TestResult
