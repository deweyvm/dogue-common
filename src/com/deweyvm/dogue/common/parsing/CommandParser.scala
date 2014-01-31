package com.deweyvm.dogue.common.parsing

import scala.util.parsing.combinator.RegexParsers
import com.deweyvm.dogue.common.Implicits._
import com.deweyvm.dogue.common.protocol.{Invalid, DogueMessage, Command}
import com.deweyvm.dogue.common.logging.Log


class CommandParser extends RegexParsers {
  override type Elem = Char
  def op = """/\w+""".r
  def word = """\w+""".r
  def arg = """[^\s\x{0}]+""".r
  def space = """[ \t\n\r\v]+""".r
  def args = rep(arg)
  def command = op~word~word~args ^^ {  case op~src~dest~args =>
      Command(op.substring(1), src, dest, args.toVector)
  }

  def parseToOpt[T](parseResult:this.ParseResult[T]): Option[T] = {
    if (parseResult.successful) {
      parseResult.get.some
    } else {
      None
    }
  }

  def parseMessage(s:String):DogueMessage = {
    val result = parseAll(command, s)
    parseToOpt(result) getOrElse Invalid(s)
  }

}
