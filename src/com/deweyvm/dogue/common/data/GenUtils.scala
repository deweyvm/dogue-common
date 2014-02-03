package com.deweyvm.dogue.common.data

import scala.util.Random

object GenUtils {
  private def rand = Random.nextInt()
  def makeGuid: String = "%08x%08x%08x%08x" format (rand, rand, rand, rand)

  def makeMiniGuid: String = "%08x%08x" format (rand, rand)

}
