package com.deweyvm.dogue.common.procgen
import scala.language.higherKinds

trait Node[T, F[_]] {
  def self:T
  def getNeighbors:F[T]
  def isNeighbor(t:T):Boolean
}

trait Graph[T, F[_]] {
  type N = Node[T,F]
  def nodes:F[N]
}
