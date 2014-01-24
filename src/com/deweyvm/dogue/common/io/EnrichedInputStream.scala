package com.deweyvm.dogue.common.io

import java.io.InputStream
import com.deweyvm.dogue.common.data.Encoding
import com.deweyvm.dogue.common.Implicits._
class EnrichedInputStream(in:InputStream) {
  def receive():Option[String] = {
    val buff = new Array[Byte](4096) //this cant be shared or it wouldnt be thread safe
    val available = in.available()
    if (available <= 0) {
      None
    } else {
      val bytesRead = in.read(buff, 0, available)
      val result = Encoding.fromBytes(buff, bytesRead)
      result.some
    }
  }
}
