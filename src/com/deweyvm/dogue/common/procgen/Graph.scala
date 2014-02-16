package com.deweyvm.dogue.common.procgen
import scala.language.higherKinds

trait Graph[T, F[_]] {
  def neighbors:F[T]
}
