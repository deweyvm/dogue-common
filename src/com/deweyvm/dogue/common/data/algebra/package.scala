package com.deweyvm.dogue.common.data


package object algebra {
  object Algebra {

    implicit class VectorWriterUtil[A](a:A) {
      def ~>[B](bool:Boolean, b: => B): EitherWriter[Vector[A], B] = {
        if (!bool) {
          EitherWriter(Vector(a), None)
        } else {
          EitherWriter(Vector(), Some(b))
        }
      }
    }
  }
}
