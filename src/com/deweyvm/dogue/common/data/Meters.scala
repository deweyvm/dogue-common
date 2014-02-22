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

  def elevationColor(max:Meters):(Color, Code) = {
    val h = d
    if (h <= 0.m) {
      val cr = Color.Blue.dim((math.abs(h)/max.d).toFloat)
      (cr, Code.≈)
    } else if (h < 50.m) {
      (Color.Yellow, Code.`.`)
    } else if (h < 750.m) {
      (Color.Green, Code.`"`)
    } else if (h < 5000.m) {
      (Color.DarkGreen, Code.♠)
    } else if (h < 6000.m) {
      (Color.DarkGrey, Code.▲)
    } else if (h < 7000.m) {
      (Color.Grey, Code.▲)
    } else if (h < 8000.m){
      (Color.White.dim(1.3f), Code.▲)
    } else if (h < 9000.m) {
      (Color.White.dim(1.2f), Code.▲)
    } else {
      (Color.White.dim(1.1f), Code.▲)
    }
  }
}
