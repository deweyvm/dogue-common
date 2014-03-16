package com.deweyvm.dogue.common.data

import com.deweyvm.dogue.common.data.algebra.Monoid
import com.deweyvm.dogue.common.CommonImplicits
import CommonImplicits._

object EitherWriter {
  def unit[W, A](value:A)(implicit m:Monoid[W]) = EitherWriter(m.zero, value.some)
  def sequence[W, A](elts:Seq[EitherWriter[W,A]])(implicit m:Monoid[W]):EitherWriter[W,Seq[A]] = {
    val first = elts.headOption.map{_.map{Seq(_)}}
    val base:EitherWriter[W,Seq[A]] = first.getOrElse(EitherWriter(m.zero, None))
    val result = elts.drop(1).foldLeft(base) { case (acc, elt) =>
      for {
        a <- acc
        e <- elt
      } yield {
         e +: a
      }
    }
    result
  }
}

case class EitherWriter[W, +A](log:W, value:Option[A]) {
  def map[B](f:A => B)(implicit m:Monoid[W]) = {
    EitherWriter(log, value.map(f))
  }

  def flatMap[B](f:A => EitherWriter[W,B])(implicit m:Monoid[W]) = {
    value match {
      case Some(a) =>
        val mapped = f(a)
        EitherWriter(m.+(log, mapped.log), mapped.value)
      case None =>
        EitherWriter(log, None)
    }
  }

  def failed(isEmpty:W => Boolean):Boolean = !isEmpty(log)

  def toEither(isEmpty:W => Boolean):Either[W, A] = value match {
    case Some(a) if isEmpty(log) => Right(a)
    case None => Left(log)
  }
}
