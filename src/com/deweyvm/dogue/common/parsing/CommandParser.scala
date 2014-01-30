package com.deweyvm.dogue.common.parsing

import scala.util.parsing.combinator.RegexParsers
import com.deweyvm.dogue.common.Implicits._
import com.deweyvm.dogue.common.protocol.Command


class CommandParser extends RegexParsers {
  override type Elem = Char
  def op = """/\w+""".r
  def word = """\w+""".r
  def space = """[ \t\n\r\v]+""".r
  def args = rep1(word)
  def end = """\x{0}""".r
  def command = op~word~word~args~end ^^ {  case op~src~dest~args~_ =>
      Command(op.substring(1), src, dest, args.toVector)
  }
}
