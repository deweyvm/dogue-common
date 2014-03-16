package com.deweyvm.dogue.common.data

import com.deweyvm.dogue.common.data.algebra.Monoid
import com.deweyvm.dogue.common.CommonImplicits
import CommonImplicits._
import scala.collection.mutable.ArrayBuffer

object Writer {
  def unit[W, A](value:A)(implicit m:Monoid[W]) = Writer(m.zero, value.some)
  def error[W,A](error:W) = Writer(error, None)
  def sequence[W, A](elts:Seq[Writer[W,A]])(implicit m:Monoid[W]):Writer[W,Seq[A]] = {
    val success = ArrayBuffer[A]()
    val fail = ArrayBuffer[W]()
    val files = ArrayBuffer[W]()
    elts foreach {
      case Writer(log, None, x) =>
        fail += log
        x foreach {files += _}
      case Writer(log, Some(a), x) =>
        success += a
        x foreach {files += _}

    }
    Writer(fail.foldLeft(m.zero)(m.+), success.toSeq.some, files.headOption)
  }
}

case class Writer[W, +A](log:W, value:Option[A], filename:Option[W]=None) {
  def fromFile(s:W):Writer[W,A] = copy(filename=s.some)
  def map[B](f:A => B)(implicit m:Monoid[W]) = {
    Writer(log, value.map(f), filename)
  }

  def flatMap[B](f:A => Writer[W,B])(implicit m:Monoid[W]) = {
    value match {
      case Some(a) =>
        val mapped = f(a)
        copy(log = m.+(log, mapped.log), value=mapped.value)
      case None =>
        copy(value=None)
    }
  }

  def prepend(w:W)(implicit m:Monoid[W]) = copy(log=m.+(w, log))

  def append(w:W)(implicit m:Monoid[W]) = copy(log=m.+(log, w))

  def failed(isEmpty:W => Boolean):Boolean = !isEmpty(log)

  def toEither(isEmpty:W => Boolean):Either[W, A] = value match {
    case Some(a) if isEmpty(log) => Right(a)
    case _ => Left(log)
  }

  def getOrCrash(isEmpty:W => Boolean, printer:W => Unit):A ={
    toEither(isEmpty) match {
      case Right(a) => a
      case _ =>
        print(printer)
        throw new RuntimeException("Loading error")
    }
  }

  def print(f:W => Unit) {
    filename foreach f
    f(log)
  }
}
