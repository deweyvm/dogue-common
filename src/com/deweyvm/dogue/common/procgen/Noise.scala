package com.deweyvm.dogue.common.procgen

import scala.util.Random
import com.deweyvm.dogue.common.data.Array2d

trait Noise {
  def render:Array2d[Double]
}
