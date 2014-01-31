package com.deweyvm.dogue.common.protocol

import com.deweyvm.dogue.common.parsing.CommandParser

object Command {


  def test() {
    val parser = new CommandParser
    val tests = List(
      ("test from to a b c", false),
      ("/test from to a b c", true),
      ("/test from                  \tto a b c", true),
      ("/test from to a", true),
      ("/test a", false),
      ("/ a b c d", false),
      ("/t a b", true)

    )



    def parse = (s:String) => parser.parseAll(parser.command, s)
    tests foreach { case (s, expected) =>
      val parsed = parse(s)
      val index = (tests map {_._1}).indexOf(s)
      assert (parsed.successful == expected,  index + " " + s + "\n" + parsed)
      parser.parseToOpt(parsed) foreach { p =>
        assert(p.toString == s.replaceAll("\\s+", " "), "\"%s\" != \"%s\"" format (p.toString, s))
      }
    }
  }
}

trait DogueMessage

case class Command(op:String, source:String, dest:String, args:Vector[String]) extends DogueMessage {
  override def toString:String = {
    if (args.length == 0) {
      "/%s %s %s" format (op, source, dest)
    } else {
      "/%s %s %s %s" format (op, source, dest, args.mkString(" "))
    }

  }

}

case class Invalid(msg:String) extends DogueMessage
