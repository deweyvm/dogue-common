package com.deweyvm.dogue.common.data

trait Indexed2d[+T] {
  def cols:Int
  def rows:Int
  def strictGetAll:Vector[T]//for debugging only
  def foreach(f:(Int,Int,T) => Unit):Unit
  def map[K](f:(Int, Int, T) => K):Indexed2d[K]

  def cut[K](c:Int, r:Int, f:T => K, default: => K):Indexed2d[K]
  def get(i:Int, j:Int):Option[T]
  def slice[K](x:Int, y:Int, width:Int, height:Int, f:T => K, default: => K):Indexed2d[K]
  def sample(div:Int):Indexed2d[T]
}
