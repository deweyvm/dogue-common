package com.deweyvm.dogue.common.procgen

import scala.collection.immutable.IndexedSeq
import scala.util.Random
import scala.collection.mutable.ArrayBuffer
import com.deweyvm.dogue.common.data.Array2d

object PerlinNoise {
  def default = {
    new PerlinNoise(1/128.0, 8, 512)
    //new PerlinNoise(1/32.0, 4, 128)
    //new PerlinNoise(1/32.0, 5, 256)
  }
}

class PerlinNoise(freq:Double, octaves:Int, val size:Int) {
  private val whatDoesThisVariableMean = size
  private val perm: IndexedSeq[Int] = {
    val t: IndexedSeq[Int] = new Random().shuffle(0 until whatDoesThisVariableMean : IndexedSeq[Int])
    t ++ t
  }
  private val pi = 3.1415926535897932384626433
  private val dirs = (0 until whatDoesThisVariableMean) map { i =>
    val x = scala.math.cos(i * 2 * pi/whatDoesThisVariableMean)
    val y = scala.math.sin(i * 2 * pi/whatDoesThisVariableMean)
    (x, y)
  }

  private def noise(x: Double, y: Double, per: Int): Double = {
    import scala.math._
    def surflet(gridX: Double, gridY: Double): Double = {
      val (distX, distY) = (abs(x - gridX), abs(y - gridY))
      def poly(v: Double) = 1 - 6*pow(v, 5) + 15*pow(v, 4) - 10*pow(v, 3)
      val polyX = poly(distX)
      val polyY = poly(distY)
      val hashed = perm(perm(gridX.toInt%per) + gridY.toInt%per)
      val grad = (x-gridX)*dirs(hashed)._1 + (y-gridY)*dirs(hashed)._2
      polyX * polyY * grad
    }
    val (intX, intY) = (x.toInt, y.toInt)
    surflet(intX,     intY    ) +
      surflet(intX + 1, intY    ) +
      surflet(intX,     intY + 1) +
      surflet(intX + 1, intY + 1)
  }

  private def fBm(x: Double, y: Double, per: Int, octs: Int): Double = {
    import scala.math._
    var v = 0.0
    for (i <- 0 until octs) {
      val p = pow(2, i).toInt
      val xArg: Double = x * p
      val yArg: Double = y * p
      v += pow(0.5, i).toFloat * noise(xArg, yArg, per * p)
    }
    v
  }





  def render:Array2d[Double] = {
    /*val freq = 1/32.0
    val size = 256
    val octs = 5*/
    val buff = ArrayBuffer[Double]()
    val result = Array2d.tabulate(size, size) { case (x, y) =>
      val r = fBm(freq*x, freq*y, (size*freq).toInt, octaves)
      buff += r
      r
    }
    val grouped = buff.groupBy(d => (d*10).toInt)
    for (i <- -10 until 10) {
      if (grouped.contains(i)) {
        print(i)
        println("    " + grouped(i).length)
      }
    }
    println("%f, %f" format (buff.max, buff.min))
    result
  }

  def printFrequencies(rendered:Array[Array[Double]]) {

  }




}
