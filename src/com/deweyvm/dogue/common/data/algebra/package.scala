package com.deweyvm.dogue.common.data

import com.deweyvm.dogue.common.CommonImplicits
import CommonImplicits._


package object algebra {
  object Algebra {
    implicit def VectorWriterUtil[A](a:A) = new {
      def ~>[B](bool:Boolean, b: => B): EitherWriter[Vector[A], B] = {
        if (!bool) {
          EitherWriter(Vector(a), Left(Vector(a)))
        } else {
          EitherWriter(Vector(), Right(b))
        }

      }
    }
  }
}
