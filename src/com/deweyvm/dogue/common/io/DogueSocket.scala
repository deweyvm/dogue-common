package com.deweyvm.dogue.common.io

import java.net.{ServerSocket, Socket}
import com.deweyvm.dogue.common.CommonImplicits._
import com.deweyvm.dogue.common.protocol.{DogueOps, Invalid, Command, DogueMessage}
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

  var nextLine = ""
  val commandQueue = new LockedQueue[DogueMessage]
  val parser = new CommandParser
  def transmit(s:DogueMessage) {
    getLogFunc(s)("Transmit: \"%s\"" format s.toString)
    synchronized {
      socket.transmit(s.toString)
    }
  }

  def ip = socket.getRemoteSocketAddress.toString

  private def receive():NetworkData[String] = synchronized {
    socket.receive()
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
      getLogFunc(cmd)("Received: \"%s\"" format cmd.toString)
    }
    commands.toVector
  }

  def setTimeout(millis:Int):Unit = synchronized {
    socket.setSoTimeout(millis)
  }

  def close() {
    try {
      socket.close()
    } catch {
      case t:Exception => ()
    }
  }



  def getLogFunc(s:DogueMessage):String => Unit = s match {
    case Command(DogueOps.Ping, _, _, _) =>
      Log.all
    case Command(DogueOps.Pong, _, _, _) =>
      Log.all
    case _ => Log.info
  }

}
