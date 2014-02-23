package com.deweyvm.dogue.common.data

import com.deweyvm.gleany.graphics.Color
import com.deweyvm.dogue.common.Implicits
import Implicits._

case class Meters(private val rep:Double) {
  def m:Meters = this
  def d = rep
  def f = rep.toFloat
  def >(other:Meters) = rep > other.rep
  def >=(other:Meters) = rep >= other.rep
  def <(other:Meters) = rep < other.rep
  def <=(other:Meters) = rep <= other.rep


}
