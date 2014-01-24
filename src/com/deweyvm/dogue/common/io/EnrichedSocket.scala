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
   * @return Some(string) where string has been read from the socket or None if no data was available
   *
   */
  def receive():Option[String] = sock.getInputStream.receive()
}
