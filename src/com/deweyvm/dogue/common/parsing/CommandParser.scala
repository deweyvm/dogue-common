package com.deweyvm.dogue.common.parsing

import scala.util.parsing.combinator.RegexParsers
import com.deweyvm.dogue.common.Implicits._
import com.deweyvm.dogue.common.protocol._
import com.deweyvm.dogue.common.protocol.Invalid


object CommandParser {
  def test() {
    val parser = new CommandParser
    val tests:List[(String, Option[Int])] = List(
      ("say a b \"preserve    whitespace\"", 1.some),
      ("pong from to a b c", 3.some),
      ("pong from                  \tto a b c", 3.some),
      ("ping from to a", 1.some),
      ("say a", None),
      (" a b c d", None),
      ("say a b", 0.some),
      ("say 6bdaeba28f26b3e3 6bdaeba28f26b3e3 ?", 1.some),
      ("says 6bdaeba28f26b3e3 6bdaeba28f26b3e3 ?", None),
      ("say 5e01405ec801cfa4 5e01405ec801cfa4 HUH?", 1.some),
      ("greet flare &unknown& identify", 1.some),
      ("say a b \"this is a test of parsing string literals\"", 1.some),
      ("local name &unknown& \"Attempting to establish a connection to dogue.in\"", 1.some)
    )



    def parse(s:String):DogueMessage = parser.getCommand(s)

    tests foreach { case (s, expected) =>
      try {
        val parsed = parse(s)
        val index = (tests map {_._1}).indexOf(s)
        assert (parsed.toOption.isDefined == expected.isDefined, index + " " + s + "\n" + parsed)
        parser.getCommand(s).toOption foreach { p =>
          expected foreach { i =>
            assert(p.args.length == i, "%d != %d" format (p.args.length, i))
          }
          //assert(p.toString == s.toString, "\"%s\" != \"%s\"" format (p.toString, s.toString))

        }
      } catch {
        case p:ParseError =>
          assert(expected.isEmpty, p.getMessage)
      }
    }
  }
}
class ParseError(msg:String) extends Exception(msg:String)
class CommandParser extends RegexParsers {
  override type Elem = Char

  def parseOp = opChoices<~"""(?!\w)""".r
  def opChoices = sayOp      |
                  pingOp     |
                  pongOp     |
                  greetOp    |
                  closeOp    |
                  nickOp     |
                  reassignOp |
                  identifyOp |
                  assignOp   |
                  localOp
  def sayOp      = "say".r      ^^ { _ => DogueOps.Say }
  def pingOp     = "ping".r     ^^ { _ => DogueOps.Ping }
  def pongOp     = "pong".r     ^^ { _ => DogueOps.Pong }
  def greetOp    = "greet".r    ^^ { _ => DogueOps.Greet }
  def closeOp    = "close".r    ^^ { _ => DogueOps.Close }
  def nickOp     = "nick".r     ^^ { _ => DogueOps.Nick }
  def reassignOp = "reassign".r ^^ { _ => DogueOps.Reassign }
  def identifyOp = "identify".r ^^ { _ => DogueOps.Identify }
  def assignOp   = "assign".r   ^^ { _ => DogueOps.Assign }
  def localOp    = "local".r   ^^ { _ => DogueOps.LocalMessage }
  def parseArg = """[^\s\x{0}"]+""".r
  def parseWord = parseString | parseArg //"""\w+""".r
  def parseString = "\"".r~>"""[^"]*""".r<~"\"".r
  def parseArgs = rep1(parseWord)
  def parseCommand: Parser[DogueMessage] = parseWord~parseWord~parseWord~parseArgs.? ^^ {  case rawOp~src~dest~args =>
    getOp(rawOp) match {
      case Right(op) => Command(op, src, dest, args map {_.toVector} getOrElse Vector())
      case Left((in, msg)) => Invalid(in, msg)
    }

  }

  def parseLocalCommand:Parser[LocalMessage] = parseWord~parseArgs.? ^^ { case rawOp~args =>
    getOp(rawOp) match {
      case Right(op) => LocalCommand(op, args map {_.toVector} getOrElse Vector())
      case Left((in, msg)) => LocalInvalid(in, msg)
    }
  }

  def getLocalCommand(input:String):LocalMessage = {
    val parseResult = parse(parseLocalCommand, input)
    parseResult.getOrElse(LocalInvalid(input, parseResult.toString))
  }

  def getCommand(input:String):DogueMessage = {
    val parseResult = parse(parseCommand, input)
    parseResult.getOrElse(Invalid(input, parseResult.toString))
  }

  def getOp(input:String):Either[(String, String), DogueOp] = {
    val parseResult = parse(parseOp, input)
    if (parseResult.successful) {
      Right(parseResult.get)
    } else {
      Left((input, parseResult.toString))
    }
  }
}
