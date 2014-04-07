package com.deweyvm.dogue.common.procgen

import scala.collection.immutable.IndexedSeq
import scala.util.Random
import com.deweyvm.dogue.common.data.{Angles, Array2d}
import scala.math._
import com.deweyvm.dogue.common.CommonImplicits
import CommonImplicits._
object PerlinNoise {
  def default = {
    new PerlinNoise(1/2048.0, 14, 4096, 0)
    //new PerlinNoise(1/128.0, 4, 1024)
    //new PerlinNoise(1/32.0, 4, 128)
    //new PerlinNoise(1/32.0, 5, 256)
    //new PerlinNoise(1/128.0, 8, 1024)
  }
}

class PerlinNoise(freq:Double, octaves:Int, val size:Int, seed:Long) extends Noise {
  def this(p:PerlinParams) = this(1/p.period.toFloat, p.octaves, p.size, p.seed)
  private val whatDoesThisVariableMean = size
  private val random = new Random(seed)
  private val perm: IndexedSeq[Int] = {
    val t: IndexedSeq[Int] = random.shuffle(0 until whatDoesThisVariableMean : IndexedSeq[Int])
    t
  }


  def getPerm(i:Int) = {
    perm((i + perm.length) % perm.length)
  }
  private val dirs = {
    ((0 until whatDoesThisVariableMean) map { i =>
      val x = scala.math.cos(i * Angles.Tau/whatDoesThisVariableMean)
      val y = scala.math.sin(i * Angles.Tau/whatDoesThisVariableMean)
      (x, y)
    }).toVector
  }

  def poly(v: Double) = 1 - 6*pow(v, 5) + 15*pow(v, 4) - 10*pow(v, 3)

  private def noise(x: Double, y: Double, per: Int): Double = {
    import scala.math._
    def surflet(gridX: Double, gridY: Double): Double = {
      val (distX, distY) = (abs(x - gridX), abs(y - gridY))
      val polyX = poly(distX)
      val polyY = poly(distY)
      val hashed = getPerm(getPerm(gridX.toInt%per) + gridY.toInt%per)
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
    (0 until octs).foldRight(0.0) { case (i, acc) =>
      val p = pow(2, i).toInt //2 << i ?
      val xArg: Double = x * p
      val yArg: Double = y * p
      acc + pow(0.5, i).toFloat * noise(xArg, yArg, per * p)
    }
  }

  def render:Array2d[Double] = {
    val raw = Array2d.parTabulate(size + size/7, size + size/7) { case (x, y) =>
      fBm(freq*(x + size/7), freq*(y + size/7), (size*freq).toInt, octaves)
    }
    val max = math.max(raw.max, -raw.min)
    val result = raw.map { case (i, j, d) =>
      d * (1/max)
    }
    printFrequencies(result).println()
    result
  }

  def printFrequencies(rendered:Array2d[Double]):String = {
    val result = new StringBuilder
    val grouped = rendered.groupBy(d => (d*10).toInt)
    (-10 to 10).filter(grouped.contains) foreach { i =>
      result append i.toString
      result append ("    " + grouped(i).length + "\n")
    }

    result append ("%.2f <=> %.2f\n" format (rendered.min, rendered.max))
    result.mkString
  }

}
