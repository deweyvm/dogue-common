package com.deweyvm.dogue.common.protocol

import com.deweyvm.dogue.common.parsing.CommandParser

object Command {
  def test() {
    val parser = new CommandParser
    val tests = List(
      ("test from to a b c\0", false),
      ("/test from to a b c\0", true),
      ("/test from                  \tto a b c\0", true),
      ("/test from to a b c", false),
      ("/test from to a\0", true),
      ("/test a\0", false),
      ("/ a b c d\0", false),
      ("/t a b\0", false)
    )

    def parseToOpt[T](parseResult:parser.ParseResult[T]): Option[T] =
      if (parseResult.successful) {
        parseResult.get.some
      } else {
        None
      }

    def parse = (s:String) => parser.parseAll(parser.command, s)
    tests foreach { case (s, expected) =>
      val parsed = parse(s)
      val index = (tests map {_._1}).indexOf(s)
      assert (parsed.successful == expected,  index + " " + s + "\n" + parsed)
      parseToOpt(parsed) foreach { p =>
        assert(p.toString == s.replaceAll("\\s+", " "), "\"%s\" != \"%s\"" format (p.toString, s))
      }
    }
  }
}

case class Command(op:String, source:String, dest:String, args:Vector[String]) {
  override def toString:String =
    "/%s %s %s %s\0" format (op, source, dest, args.mkString(" "))
}
