package com.deweyvm.dogue.common.procgen

import java.awt.image.BufferedImage
import util.Random
import java.awt.Color
import javax.imageio.ImageIO
import java.io.File
import scala.collection.mutable.ListBuffer
import com.deweyvm.dogue.common.CommonImplicits
import CommonImplicits._
import com.deweyvm.gleany.graphics
import com.deweyvm.gleany.data.Point2i


object NaiveVoronoi {
  def main(args: Array[String]) {
    //Voronoi.generateVoronoi(4096,4096,30, manhattan)
  }

  def writeImage(fileName: String, image: BufferedImage): Unit = {
    val output = new File(fileName)
    ImageIO.write(image, "png", output).ignore()
  }

  def manhattan(p1:Point2i, p2:Point2i):Double = {
    math.abs(p2.x - p1.x) + math.abs(p2.y - p1.y)
  }

  def euclidian(p1:Point2i, p2:Point2i):Double = {
    math.hypot(p2.x-p1.x, p2.y-p1.y)
  }

  def generateVoronoi(width:Int,height:Int,numCells:Int, dist:(Point2i, Point2i) => Double, seed:Long)  {
    val random = new Random(seed)
    val image = new BufferedImage(width,height,BufferedImage.TYPE_INT_RGB)

    def putPixel(x:Int, y:Int, rgb: (Int,Int,Int)) = {
      val (r,g,b) = rgb
      image.setRGB(x,y,new Color(r,g,b).getRGB)
    }

    val points = (0 until numCells).map { (_:Any) => Point2i(random.nextInt(width), random.nextInt(height))}.toVector

    val colors = (0 until numCells).map {_ => graphics.Color.randomHue() }.toVector


    for (y <- 0 until height) {
      for (x <- 0 until width) {
        val pt = Point2i(x, y)
        val dists = (0 until numCells) map { k =>
          val p = points(k)
          dist(p, pt)
        }
        val min = dists.min
        val index = dists.indexOf(min)
        putPixel(x,y,colors(index).toByteTuple)
      }
    }
    writeImage("voronoi.png", image)
  }
}
