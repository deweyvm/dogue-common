package com.deweyvm.dogue.common.data


package object algebra {
  object Algebra {


    implicit class VectorWriterUtil[A](a:A) {
      def ~|>[B](bool:Boolean, b: => B): Writer[Vector[A], B] = {
        if (!bool) {
          Writer(Vector(a), None)
        } else {
          Writer(Vector(), Some(b))
        }
      }
    }


  }
}
