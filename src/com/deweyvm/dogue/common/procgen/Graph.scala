package com.deweyvm.dogue.common.procgen
import scala.language.higherKinds

trait Node[T, F[_]] {
  def self:T
  def getNeighbors:F[T]
  def isNeighbor(t:T):Boolean
}

object Graph {
  def createVec[T](n:Map[T, Node[T, Vector]]):Graph[T,Vector] = {
    val values = n.values.toVector
    new Graph[T, Vector] {
      def nodes:Vector[N] = values
      def getNode(t:T):Option[N] = n.get(t)
    }
  }

}

trait Graph[T, F[_]] {
  type N = Node[T,F]
  def nodes:F[N]
  def getNode(t:T):Option[N]
}
