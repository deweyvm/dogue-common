package com.deweyvm.dogue.common.io

import java.net.{ServerSocket, Socket}
import com.deweyvm.dogue.common.threading.Lock
import com.deweyvm.dogue.common.Implicits._
import com.deweyvm.dogue.common.protocol.{DogueMessage, Command}
import scala.collection.mutable.ArrayBuffer
import com.deweyvm.dogue.common.parsing.CommandParser
import com.deweyvm.dogue.common.data.LockedQueue
import com.deweyvm.dogue.common.logging.Log


class DogueServer(name:String, port:Int) {
  private val server = new ServerSocket(port)
  def accept():DogueSocket = {
    new DogueSocket(name, server.accept())
  }
}

object DogueSocket {
  def create(serverName:String, host:String, port:Int) =
    new DogueSocket(serverName, new Socket(host, port))
}

class DogueSocket(val serverName:String, socket:Socket) {
  val readLock = new Lock()
  val writeLock = new Lock()
  var nextLine = ""
  val commandQueue = new LockedQueue[DogueMessage]
  val parser = new CommandParser
  def transmit(s:DogueMessage) {
    writeLock.map(socket.transmit)(s.toString)
  }

  private def receive():NetworkData[String] = {
    readLock.get(socket.receive)
  }

  //in the future might want to have this run more often than it is queried
  private def accept() {
    val buffer = ArrayBuffer[String]()
    receive() foreach { next =>
      val lines = next.esplit('\0')
      val last = lines(lines.length - 1)
      val first = lines.dropRight(1)
      for (s <- first) {
        nextLine += s
        buffer += nextLine
        nextLine = ""
      }

      nextLine = last
    }
    commandQueue.enqueueAll((buffer map parser.parseMessage).toVector)
  }

  def receiveCommands():Vector[DogueMessage] = {
    accept()
    commandQueue.dequeueAll()
  }

  def setTimeout(millis:Int) {
    readLock.map(socket.setSoTimeout)(millis)
  }

}
