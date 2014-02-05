package com.deweyvm.dogue.common.protocol

trait DogueOp {
  override def toString = getClass.getSimpleName.toLowerCase.replace("$","")
}


object DogueOps {
  def getAll = List(Say, Ping, Pong, Greet, Nick, Assign, Reassign, Identify, Close, LocalMessage)
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
