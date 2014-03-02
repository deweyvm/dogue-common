package com.deweyvm.dogue.common.data

trait Indexed2d[T] {
  def cols:Int
  def rows:Int
  def strictGetAll:Vector[T]//for debugging only
  def foreach(f:(Int,Int,T) => Unit):Unit
  def map[K](f:(Int, Int, T) => K):Indexed2d[K]
  def zip[K](a:Indexed2d[K], default: K):Indexed2d[(T,K)] = {
    map { case (i, j, t) =>
      (t, a.get(i, j).getOrElse(default))
    }
  }
  def cut(c:Int, r:Int, default:T):Indexed2d[T]
  def get(i:Int, j:Int):Option[T]
  def slice(x:Int, y:Int, width:Int, height:Int, default:T):Indexed2d[T]
  def sample(div:Int):Indexed2d[T]
}
