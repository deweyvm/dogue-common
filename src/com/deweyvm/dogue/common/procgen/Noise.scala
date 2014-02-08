package com.deweyvm.dogue.common.procgen

import scala.util.Random

class Noise(cols:Int, rows:Int, seed:Long) {

  def get(x:Int, y:Int):Int = {
    val h = hash(x, y)
    Random.setSeed(h + seed)
    Random.nextInt()
  }

  private def hash(x:Int, y:Int):Long = (x << 32) | y
}
