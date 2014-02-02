package com.deweyvm.dogue.common.protocol

import com.deweyvm.dogue.common.parsing.CommandParser

trait DogueOp {
  override def toString = getClass.getSimpleName.toLowerCase.replace("$","")
}
object DogueOp {
  case object Say extends DogueOp
  case object Ping extends DogueOp
  case object Pong extends DogueOp
  case object Greet extends DogueOp
  case object Quit extends DogueOp
}


trait DogueMessage

case class Command(op:DogueOp, source:String, dest:String, args:Vector[String]) extends DogueMessage {
  override def toString:String = {
    val front = "%s %s %s" format (op, source, dest)
    if (args.length == 0) {
      front
    } else {
      "%s %s" format (front, args.mkString(" "))
    }
  }

  def this(op:DogueOp, source:String, dest:String, args:String*) =
    this(op, source, dest, args.toVector)

  def toSay:String = {
    args.mkString(" ")
  }
}

case class Invalid(text:String) extends DogueMessage {
  override def toString:String = "Invalid(%s)" format text
}
