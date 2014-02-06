package com.deweyvm.dogue.common.data

object Encoding {
  private val charset = "UTF-8"
  def toBytes(string:String) = string.getBytes(charset)
  def fromBytes(bytes:Array[Byte], len:Int) = new String(bytes, 0, len, charset)
}
