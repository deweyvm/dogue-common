package com.deweyvm.dogue.common.io

import java.net.{ServerSocket, Socket}
import com.deweyvm.dogue.common.threading.Lock
import com.deweyvm.dogue.common.Implicits._


class DogueServer(port:Int) {
  private val server = new ServerSocket(port)
  def accept():DogueSocket = {
    new DogueSocket(server.accept())
  }
}

object DogueSocket {
  def create(host:String, port:Int) = new DogueSocket(new Socket(host, port))
}

class DogueSocket(socket:Socket) {
  val readLock = new Lock()
  val writeLock = new Lock()

  def transmit(s:String) {
    writeLock.map(socket.transmit)(s)
  }

  def receive():NetworkData = {
    readLock.get(socket.receive)
  }

  def setTimeout(millis:Int) {
    readLock.map(socket.setSoTimeout)(millis)
  }

}
