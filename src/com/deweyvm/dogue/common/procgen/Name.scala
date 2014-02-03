package com.deweyvm.dogue.common.procgen

import com.deweyvm.dogue.common.Implicits._

class Name {
  val prefixes = Vector("shibe", "doge", "so", "very", "such", "wow", "much")
  val suffixes = Vector("smell", "run", "tumble", "roll", "belch")
  def get:String = {
    prefixes.pickN(2).mkString + suffixes.getRandom
  }
}
