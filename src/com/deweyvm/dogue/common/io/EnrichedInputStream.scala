package com.deweyvm.dogue.common.io

import java.io.InputStream
import com.deweyvm.dogue.common.data.Encoding
import com.deweyvm.dogue.common.Implicits._
import com.deweyvm.dogue.common.io
import scala.annotation.tailrec
import com.deweyvm.dogue.common.io.NetworkData.{Data, NoneAvailable}
import com.deweyvm.dogue.common.logging.Log

class NetworkData[+T](val isEmpty:Boolean) {
  def foreach(f:T=>Unit) = ()
}
object NetworkData {
  case object EndOfStream extends NetworkData[Nothing](true)
  case object NoneAvailable extends NetworkData[Nothing](true)
  case class Data[T](string:T) extends NetworkData[T](false) {
    override def foreach(f:T=>Unit) = f(string)
    override def toString = string.toString
  }

  def concat(x:NetworkData[String], y:NetworkData[String]):NetworkData[String] = {
    y match {
      case EndOfStream => x
      case NoneAvailable => x
      case Data(s) =>
        x match {
          case Data(r) => Data(r + s)
          case _ => x
        }
    }
  }
}

class EnrichedInputStream(in:InputStream) {
  def receive():NetworkData[String] = {
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
  def receiveAll():NetworkData[String] = {
    receiveNext(NetworkData.Data(""))
  }

  @tailrec
  private def receiveNext(last:NetworkData[String]):NetworkData[String] = {
    import NetworkData._
    val buffLen = 4096
    val buff = new Array[Byte](buffLen)
    Log.info("Attempting read")
    val amountRead = in.read(buff, 0, buffLen)
    Log.info("Read " + amountRead + " bytes")
    if (amountRead <= 0) {
      last
    } else {
      val next = concat(last, Data(Encoding.fromBytes(buff, amountRead)))
      receiveNext(next)
    }
  }
}
