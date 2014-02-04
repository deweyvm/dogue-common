package com.deweyvm.dogue.common.protocol

import com.deweyvm.dogue.common.parsing.CommandParser
import com.deweyvm.dogue.common.Implicits._
import com.deweyvm.dogue.common.logging.Log

trait DogueOp {
  override def toString = getClass.getSimpleName.toLowerCase.replace("$","")
}
object DogueOps {
  case object Say extends DogueOp
  case object Ping extends DogueOp
  case object Pong extends DogueOp
  case object Greet extends DogueOp
  case object Nick extends DogueOp

  /**
   * Assign a username and password to a user who has requested one.
   */
  case object Assign extends DogueOp

  /**
   * Forcibly assign a name to a user due to conflicts or issues.
   */
  case object Reassign extends DogueOp
  case object Identify extends DogueOp
  case object Close extends DogueOp
  case object LocalMessage extends DogueOp {
    override def toString = "local"
  }

}


trait DogueMessage {
  def toOption:Option[Command]
}

case class LocalCommand(op:DogueOp, args:Vector[String]) {
  def toCommand(src:String, dest:String) = Command(op, src, dest, args)
}

case class Command(op:DogueOp, source:String, dest:String, args:Vector[String]) extends DogueMessage {
  override def toString:String = {
    val front = "%s %s %s" format (op, source, dest)
    if (args.length == 0) {
      front
    } else {
      "%s %s" format (front, args map { a =>
        val unquoted = a//.replace("\"", "'")
        if (unquoted.contains(' ')) {
          "\"%s\"" format unquoted
        } else {
          unquoted
        }
      } mkString " ")
    }

  }

  def this(op:DogueOp, source:String, dest:String, args:String*) =
    this(op, source, dest, args.toVector)

  override def toOption = this.some
}

case class Invalid(text:String, parseMessage:String) extends DogueMessage {
  override def toString:String = "Invalid(%s): %s" format(text, parseMessage)
  override def toOption = None
  def warn() {
    Log.warn("Invalid command: \"%s\"\n%s" format (text, parseMessage))
  }
}
