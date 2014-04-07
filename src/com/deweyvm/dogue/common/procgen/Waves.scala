package com.deweyvm.dogue.common.procgen

import java.util.Random
import com.deweyvm.dogue.common.data.{Array2d, Angles}

case class Wave(amp:Double, x0:Double, y0:Double) {
  def apply(x:Double, y:Double) = {
    import scala.math._
    sin(Angles.Tau/amp * hypot(x - x0, y - y0))
  }
}

class Waves(seed:Long, rows:Int, cols:Int) /*extends Noise*/ {
  val r = new Random(seed)
  val numWaves = r.nextInt(5) + 10
  val waves = (0 until numWaves) map { _ =>

    val amp = 0.5 + 0.5*r.nextDouble()
    val x = (r.nextDouble() - 0.5)*2
    val y = (r.nextDouble() - 0.5)*2
    Wave(amp, x, y)
  }
  def render = Array2d.tabulate[Double](rows, cols) { case (i, j) =>
    waves.foldLeft(0.0) { _ + _(i, j)}
  }
}
