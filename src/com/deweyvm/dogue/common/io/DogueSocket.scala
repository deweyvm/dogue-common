package com.deweyvm.dogue.common.io

import java.net.{ServerSocket, Socket}
import com.deweyvm.dogue.common.threading.Lock
import com.deweyvm.dogue.common.Implicits._
import com.deweyvm.dogue.common.protocol.{Invalid, Command, DogueMessage}
import scala.collection.mutable.ArrayBuffer
import com.deweyvm.dogue.common.parsing.CommandParser
import com.deweyvm.dogue.common.data.LockedQueue
import com.deweyvm.dogue.common.logging.Log


class DogueServer(name:String, port:Int) {
  private val server = new ServerSocket(port)
  def accept():DogueSocket = {
    new DogueSocket(name, server.accept())
  }

  def reuseAddress() {
    server.setReuseAddress(true)
  }

  def setTimeout(millis:Int) {
    server.setSoTimeout(millis)
  }

  def close() {
    server.close()
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
    Log.info("Transmit: \"%s\"" format s.toString)
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
    commandQueue.enqueueAll((buffer map parser.getCommand).toVector)
  }

  def receiveCommands():Vector[Command] = {
    accept()

    val all = commandQueue.dequeueAll()
    val commands = ArrayBuffer[Command]()
    all foreach {
      case cmd@Command(_,_,_,_) => commands += cmd
      case inv@Invalid(_,_) => inv.warn()
    }

    commands foreach {cmd =>
      cmd.toString foreach { c =>
        println(c.toInt)

      }
      Log.info("Received: \"%s\"" format cmd.toString)
    }
    commands.toVector
  }

  def setTimeout(millis:Int) {
    readLock.map(socket.setSoTimeout)(millis)
  }

  def close() {
    try {
      socket.close()
    }
  }

}
