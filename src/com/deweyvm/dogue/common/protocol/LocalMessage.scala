package com.deweyvm.dogue.common.protocol

import com.deweyvm.dogue.common.Implicits
import Implicits._

trait LocalMessage {
  def toDogueMessage(src:String, dest:String):DogueMessage
  def toOption:Option[LocalCommand] = None
}

case class LocalCommand(op:DogueOp, args:Vector[String]) extends LocalMessage {
  override def toDogueMessage(src:String, dest:String) = Command(op, src, dest, args)
  override def toOption = this.some
  def this(op:DogueOp, args:String*) = this(op, args.toVector)
}

case class LocalInvalid(line:String, msg:String) extends LocalMessage {
  override def toDogueMessage(src:String, dest:String) = Invalid(line, msg)
}
