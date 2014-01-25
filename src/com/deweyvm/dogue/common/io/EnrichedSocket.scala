package com.deweyvm.dogue.common.io

import java.net.Socket
import com.deweyvm.dogue.common.data.Encoding
import com.deweyvm.dogue.common.Implicits._
//enriched socket for communicating over the dogue server protocol
class EnrichedSocket(sock:Socket) {
  /**
   * requires: same requirements as sock.getInputStream and stream.write
   * does not catch any exceptions
   */
  def transmit(string:String) {
    sock.getOutputStream.transmit(string)
  }

  /**
   * does not catch any exceptions
   *
   */
  def receive():NetworkData = sock.getInputStream.receive()

  def receiveAll():NetworkData = sock.getInputStream.receiveAll()
}
