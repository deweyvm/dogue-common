package com.deweyvm.dogue.common.data

import scala.collection.mutable.ArrayBuffer
import com.deweyvm.gleany.graphics.Color
import java.util.Random
import com.deweyvm.dogue.common.CommonImplicits
import CommonImplicits._
object ColorHarmony {
  def create(numColors:Int, startHue:Double, r0:Double, r1:Double, r2:Double, offset1:Double, offset2:Double, sat:Double, lum:Float, seed:Long):Vector[Color] = {
    val random = new Random(seed)
    (0 until numColors).map { i =>
      val randomAngle = random.nextDouble * (r0 + r1 + r2)
      val offset = if (randomAngle > r0) {
        (randomAngle < r0 + r1).select(offset1, offset2)
      } else {
        0
      }
      val hue = (startHue + randomAngle + offset) % 1.0f
      Color.fromHsb(hue.toFloat, sat.toFloat, lum.toFloat)
    }.toVector
  }
}
