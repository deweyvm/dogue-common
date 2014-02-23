package com.deweyvm.dogue.common.data


case class Meters(d:Double) extends AnyVal {
  def m:Meters = this
  def f = d.toFloat
  def >(other:Meters) = d > other.d
  def >=(other:Meters) = d >= other.d
  def <(other:Meters) = d < other.d
  def <=(other:Meters) = d <= other.d
  def unary_- = Meters(-d)

}
