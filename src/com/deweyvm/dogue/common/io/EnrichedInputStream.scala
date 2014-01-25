package com.deweyvm.dogue.common.io

import java.io.InputStream
import com.deweyvm.dogue.common.data.Encoding
import com.deweyvm.dogue.common.Implicits._
import com.deweyvm.dogue.common.io
import scala.annotation.tailrec
import com.deweyvm.dogue.common.io.NetworkData.NoneAvailable

class NetworkData(val isEmpty:Boolean) {

  def concat(other:NetworkData) = other
  def foreach(f:String=>Unit) = ()
}
object NetworkData {
  case object EndOfStream extends NetworkData(true)
  case object NoneAvailable extends NetworkData(true)
  case class Data(string:String) extends NetworkData(false) {
    override def concat(other:NetworkData) = other match {
      case Data(s) => Data(s + string)
      case _ => this
    }
    override def foreach(f:String=>Unit) = f(string)
  }
}

class EnrichedInputStream(in:InputStream) {
  def receive():NetworkData = {
    val buff = new Array[Byte](4096) //this cant be shared or it wouldnt be thread safe
    val available = in.available()
    if (available == -1) {
      NetworkData.EndOfStream
    } else if (available <= 0) {
      NetworkData.NoneAvailable
    } else {
      val bytesRead = in.read(buff, 0, available)
      val result = Encoding.fromBytes(buff, bytesRead)
      NetworkData.Data(result)
    }
  }

  /**
   * NOTE: stream must be blocking
   * @return
   */
  def receiveAll():NetworkData = {
    receiveNext(NetworkData.Data(""))
  }

  @tailrec
  private def receiveNext(last:NetworkData):NetworkData = {
    import NetworkData._
    val buffLen = 4096
    val buff = new Array[Byte](buffLen)
    val amountRead = in.read(buff, 0, buffLen)
    if (amountRead == 0) {
      last
    } else {
      val next = last concat Data(Encoding.fromBytes(buff, amountRead))
      receiveNext(next)
    }
  }
}
