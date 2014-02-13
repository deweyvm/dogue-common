package com.deweyvm.dogue.common.procgen

class PerlinPoisson(val size:Int, val minDist:Int, freq:Double, octaves:Int, seed:Int) {
  private val perlin = new PerlinNoise(freq, octaves, size, seed).lazyRender
  val rng = new PoissonRng(size,size, {
    case (i, j) => perlin.get(i, j).map{t =>
      val x = t*35
      math.max(x, minDist)
    }.getOrElse(10)
  }, minDist)


}
