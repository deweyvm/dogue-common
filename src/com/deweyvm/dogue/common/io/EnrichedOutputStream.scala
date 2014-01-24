package com.deweyvm.dogue.common.io

import java.io.OutputStream
import com.deweyvm.dogue.common.data.Encoding

class EnrichedOutputStream(out:OutputStream) {
  def transmit(string:String) {
    out.write(Encoding.toBytes(string + "\0"))
  }
}
