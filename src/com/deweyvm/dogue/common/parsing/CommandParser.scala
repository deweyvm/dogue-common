package com.deweyvm.dogue.common.parsing

import scala.util.parsing.combinator.RegexParsers
import com.deweyvm.dogue.common.Implicits._
import com.deweyvm.dogue.common.protocol._
import com.deweyvm.dogue.common.protocol.Invalid


object CommandParser {
  def test() {
    val parser = new CommandParser
    val tests = List(
      ("pong from to a b c", true),
      ("pong from                  \tto a b c", true),
      ("ping from to a", true),
      ("say a", false),
      (" a b c d", false),
      ("say a b", true),
      ("say 6bdaeba28f26b3e3 6bdaeba28f26b3e3 ?", true),
      ("says 6bdaeba28f26b3e3 6bdaeba28f26b3e3 ?", false),
      ("say 5e01405ec801cfa4 5e01405ec801cfa4 HUH?", true),
      ("greet flare &unknown& identify", true)

    )



    def parse(s:String) = parser.parseAll(parser.command, s)

    tests foreach { case (s, expected) =>
      try {
        val parsed = parse(s)
        val index = (tests map {_._1}).indexOf(s)
        assert (parsed.successful == expected,  index + " " + s + "\n" + parsed)
        parser.parseToOpt(parsed) foreach { p =>
          assert(p.toString == s.replaceAll("\\s+", " "), "\"%s\" != \"%s\"" format (p.toString, s))
        }
      } catch {
        case p:ParseError =>
          assert(!expected, p.getMessage)
      }
    }
  }
}
class ParseError(msg:String) extends Exception(msg:String)
class CommandParser extends RegexParsers {

  override type Elem = Char



  def parseOp = sayOp | pingOp | pongOp | greetOp
  def sayOp = """say""".r ^^ { _ => DogueOp.Say }
  def pingOp = """ping""".r ^^ { _ => DogueOp.Ping }
  def pongOp = """pong""".r ^^ { _=> DogueOp.Pong }
  def greetOp = """greet""".r ^^ { _ => DogueOp.Greet }
  def parseArg = """[^\s\x{0}]+""".r
  def parseWord = parseArg//"""\w+""".r
  //def space = """[ \t\n\r\v]+""".r
  def parseArgs = rep(parseArg)
  def command: Parser[Command] = parseWord~parseWord~parseWord~parseArgs ^^ {  case rawOp~src~dest~args =>

      parseToOpt(parse(phrase(parseOp<~"""\z""".r), rawOp)) map { op =>
        Command(op, src, dest, args.toVector)
      } getOrElse {throw new ParseError("Unknown opcode: " + rawOp + " in " + "%s %s %s %s" format (rawOp, src, dest, args.mkString(" ")))}

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
