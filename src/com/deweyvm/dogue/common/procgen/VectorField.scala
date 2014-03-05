package com.deweyvm.dogue.common.procgen

import com.deweyvm.gleany.data.{Point2i, Point2d}
import com.deweyvm.dogue.common.Implicits
import Implicits._
import com.deweyvm.gleany.graphics.Color
import scala.util.Random
import com.deweyvm.dogue.common.data.{Array2dView, Array2d, Angles}

object VectorField {

  def test() {
    def takeGradient(ones:Vector[(Int,Int)]) = {
      gradient(Array2d.tabulate(3,3) {case (i, j)  =>
        if (ones.contains((i, j))) {
          1
        } else {
          0
        }
      }, 1, 1)
    }




    val tests = List(
      (Vector((1,1)), Point2d(0,0)),
      (Vector((0,0), (2,2)), Point2d(0,0)),
      (Vector((0,2), (2,0)), Point2d(0,0)),
      (Vector((0,2), (2,0), (1,1)), Point2d(0,0)),
      (Vector((0,2), (0,0)), Point2d(-0.25,0)),
      (Vector((2,0), (0,0)), Point2d(0,-0.25)),
      (Vector((2,0), (2,2)), Point2d(0.25,0)),
      (Vector((0,2), (2,2)), Point2d(0, 0.25))

    )

    tests foreach { case (ones, expected) =>
      val grad = takeGradient(ones)
      assert(expected == grad, "Expected (%s) got (%s) for (%s)" format (expected, grad, ones))
    }
  }

  /**
   * out of bounds elements are treated as the same as the center tile
   * @param array
   * @param i
   * @param j
   * @return
   */
  def gradient(array:Array2d[Double], i:Int, j:Int):Point2d = {
    if (i - 1 < 0 || i + 1 > array.cols - 1 || j - 1 < 0 || j + 1 > array.rows - 1) {
      return Point2d.Zero
    }
    val grid: Array2dView[Double] = array.view.slice(i-1, j-1, 3, 3)
    def p(x:Double, y:Double) = Point2d(x, y)

    val c11:Double = grid.get(1,1)
    val c00:Double = grid.get(0,0)
    val c01:Double = grid.get(0,1)
    val c02:Double = grid.get(0,2)
    val c10:Double = grid.get(1,0)
    val c12:Double = grid.get(1,2)
    val c20:Double = grid.get(2,0)
    val c21:Double = grid.get(2,1)
    val c22:Double = grid.get(2,2)
    val v00 = (c11 - c00) *: (p(1,1) - p(0,0))
    val v01 = (c11 - c01) *: (p(1,1) - p(0,1))
    val v02 = (c11 - c02) *: (p(1,1) - p(0,2))
    val v10 = (c11 - c10) *: (p(1,1) - p(1,0))
    //val v11 = (c11 - c11) *: (p(1,1) - p(1,1)) //this is always zero
    val v12 = (c11 - c12) *: (p(1,1) - p(1,2))
    val v20 = (c11 - c20) *: (p(1,1) - p(2,0))
    val v21 = (c11 - c21) *: (p(1,1) - p(2,1))
    val v22 = (c11 - c22) *: (p(1,1) - p(2,2))
    (v00 + v01 + v02 + v10 /*+ v11*/ + v12 + v20 + v21 + v22)/8

  }


  def windSpiral(width:Int, height:Int, scale:Int) = {
    val r = new Random(0)
    val land = (0 until 4) map {_ => Point2d((r.nextDouble() - 0.5)*width/scale*2,
                                             (r.nextDouble() - 0.5)*height/scale*2)}
    val max = Point2d(width, height).magnitude/(scale*2)

    def pow(k:Double) = math.pow(k, 1.15)
    def getInfluence(i:Double, j:Double):Point2d = {
      val mp = Point2d(i, j)
      val vectors = land.filter { (pt:Point2d) =>
        (pt - mp).magnitude < 50
      }.map { (pt:Point2d) =>
        val dir = mp.normalize
        (20/(pt - mp).magnitude.sqrt) *: dir
      }

      vectors.foldLeft(Point2d.Zero) {case (acc, pt) => acc + pt}
    }
    def dd(i:Double, j:Double):Point2d = {
      val p = Point2d(i, j)
      val mag = pow(max) - pow(p.magnitude)
      Point2d(mag * p.normalize.y, -mag * p.normalize.x) + getInfluence(i, j)
    }

    new VectorField(-width, -height, 2*width, 2*height, scale, dd)
  }

  def perlin(width:Int, height:Int, scale:Int) = {
    val noise = new PerlinNoise(1/32.0, 5, width, 0L).render
    def getInfluence(i:Double, j:Double):Point2d = {
      val grad = gradient(noise, i.toInt + width/scale, j.toInt + height/scale)
      600 *: grad.rotate(Angles.Tau/4)
    }
    def dd(i:Double, j:Double):Point2d = {
      getInfluence(i, j)
    }

    new VectorField(-width, -height, 2*width, 2*height, scale, dd)
  }

  def perlinWind(solidElevation:Double, noise:Array2d[Double], width:Int, height:Int, scale:Int, seed:Long) = {
    def pow(k:Double) = math.pow(k, 1.25)
    def getInfluence(i:Double, j:Double):Point2d = {
      val atPoint = noise.get(i.toInt, j.toInt).getOrElse(0.0)
      val factor =
        if (atPoint < solidElevation) {
          math.abs(atPoint).clamp(0,0.02)
        } else {
          atPoint.clamp(0,0.12)
        }
      gradient(noise, i.toInt, j.toInt).normalize.rotate(-Angles.Tau/2)*(factor/5)
    }
    def xx(i:Double) = 0.5 - i/width
    def yy(j:Double) = 0.5 - j/height
    def dd(i:Double, j:Double):Point2d = {
      val x = xx(i)
      val y = yy(j)
      val p = Point2d(x, y)
      val mag = pow(0.3) - pow(p.magnitude)
      val norm = p.normalize
      val influence = getInfluence(i, j)
      val result = (mag *: Point2d(norm.y, -norm.x)) + influence
      result*2
    }
    new VectorField(0, 0, width, height, scale, dd)
  }



  def perlinWindOrig(width:Int, height:Int, scale:Int, seed:Long) = {
    val max = Point2d(width, height).magnitude/(scale*2)

    def pow(k:Double) = math.pow(k, 1.15)
    val noise = new PerlinNoise(1/32.0, 5, width, seed).render
    def getInfluence(i:Double, j:Double):Point2d = {
      val grad = gradient(noise, i.toInt + width/scale, j.toInt + height/scale)
      grad.rotate(-Angles.Tau/4).normalize * 20
    }
    def dd(i:Double, j:Double):Point2d = {
      val p = Point2d(i, j)
      val mag = pow(max) - pow(p.magnitude)
      mag *: Point2d(p.normalize.y, -p.normalize.x) + getInfluence(i, j)
    }
    new VectorField(-width, -height, 2*width, 2*height, scale, dd)
  }

  def reverseSpiral(width:Int, height:Int, scale:Int) = {
    def dd(i:Double, j:Double):Point2d = {
      val x = i
      val y = j
      val mag = math.pow(Point2d(x, y).magnitude, 1.25)
      Point2d(y, -x)*(scale/mag)

    }
    new VectorField(-width, -height, 2*width, 2*height, scale, dd)
  }

  def simpleSpiral(width:Int, height:Int) = {
    def dd(i:Double, j:Double):Point2d = {
      val x = width/2 - i
      val y = height/2 - j
      val mag = Point2d(x, y).magnitude
      Point2d(y*mag, -x*mag)
    }
    new VectorField(-width, -height, 2*width, 2*height, 40, dd)
  }

  def magToColor(mag:Double):Color = {
    //Color.fromHsb((mag.toFloat/100 + 0.45f) % 1)
    Color.fromHsb((mag.toFloat + 0.35f) % 1)
  }

}

class VectorField(x:Int, y:Int, width:Int, height:Int, div:Int, dd:(Double, Double) => Point2d) {
  lazy val vectors = {
    for (i <- x/div until (x + width)/div;
         j <- y/div until (y + height)/div) yield {
      getArrow(i, j)
    }
  }

  private def getArrow(i:Double, j:Double) = {
    val v = dd(i, j)
    val mag = v.magnitude.clamp(0.01,50)
    val color = VectorField.magToColor(mag)
    val d = v.normalize
    (Point2d(i*div, j*div), Arrow(d, mag), color)
  }

  val lazyVectors: Array2d[(Point2d, Arrow, Color)] = Array2d.tabulate(width, height) {case (i, j) =>
    getArrow(i, j)
  }

}
