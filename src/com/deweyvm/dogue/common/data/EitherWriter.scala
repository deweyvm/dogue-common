package com.deweyvm.dogue.common.data

import com.deweyvm.dogue.common.data.algebra.Monoid



case class EitherWriter[W, A](log:W, value:Either[W, A]) {
  def map[B](f:A => B) = EitherWriter(log, value.right.map(f))

  def flatMap[B](f:A => EitherWriter[W,B])(implicit m:Monoid[W]) = {
    val x:EitherWriter[W,B] = value match {
      case Right(a) => f(a)
      case Left(err) =>  EitherWriter(m.+(log, err), Left(err))
    }
    x
  }

  def failed(isEmpty:W => Boolean):Boolean = !isEmpty(log)
}
