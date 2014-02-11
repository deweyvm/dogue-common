package com.deweyvm.dogue.common.procgen

import com.deweyvm.dogue.common.Implicits._
import scala.util.Random
import scala.language.implicitConversions
object Name {
  val unknown = "&unknown&"
}

class Name {
  val prefixes = Vector("shibe", "doge", "so", "very", "such", "wow", "much")
  val suffixes = Vector("smell", "run", "tumble", "roll", "belch")
  def get:String = {
    prefixes.pickN(2).mkString + suffixes.getRandom
  }
}


class MapName(seed:Long) {
  private val random = new Random(seed)

  private class Gettable[T](self:Vector[T]) {
    def get:T = self.getRandom(random)
  }
  private implicit def vecToGettable[T](a:Vector[T]) = new Gettable(a)


  trait Letter {
    def getLetter:String

  }
  case object Consonant extends Letter {
    val consonants = "bcdfghjklmnpqrstvwxyz".toVector
    override def getLetter = consonants.get + ""
  }
  case object Vowel extends Letter {
    val vowels = "aeiouy".toVector
    override def getLetter = vowels.get + ""
  }

  case object PreConsonant extends Letter {
    val pre = "nfcnmrs".toVector
    def getLetter = {
      if (random.nextDouble() < 0.5) {
        pre.get + ""
      } else {
        ""
      }
    }
  }

  def makeName = (getPrefix + getInfix + getSuffix).capitalize

  private def getPrefix = {
    val types = Vector(List(Vowel, Consonant),
                       List(Vowel, Vowel),
                       List(Consonant, Vowel),
                       List(Vowel, Consonant, Vowel)
    )
    val t = types.get
    (t map { _.getLetter}).mkString
  }

  private def getInfix = {
    val types = Vector(List(Consonant, Vowel, PreConsonant, Consonant))
    (types(0) map { _.getLetter}).mkString
  }

  private def getSuffix = {
    Vector("a", "ia", "oa", "atol", "edow", "nia", "tia").get
  }
}
