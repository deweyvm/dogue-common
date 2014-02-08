package com.deweyvm.dogue

import java.net.Socket
import scala.language.implicitConversions
import com.deweyvm.dogue.common.data._
import com.deweyvm.dogue.common.io.{EnrichedInputStream, EnrichedOutputStream, EnrichedSocket}
import java.io.{OutputStream, InputStream}
import scala.language.experimental.macros


package object common {
  object Implicits {
    implicit def any2Option[A](x: A):EnrichedOption[A] = new EnrichedOption(x)
    implicit def number2EnrichedNumber[A](rep:A)(implicit n:Numeric[A]) = new EnrichedNumber(rep)
    implicit def string2EnrichedString(x:String):EnrichedString = new EnrichedString(x)
    implicit def socket2EnrichedSocket(sock:Socket):EnrichedSocket = new EnrichedSocket(sock)
    implicit def outputStream2EnrichedOutputStream(out:OutputStream):EnrichedOutputStream =
      new EnrichedOutputStream(out)
    implicit def inputStream2EnrichedInputStream(in:InputStream):EnrichedInputStream =
      new EnrichedInputStream(in)
    implicit def indexedSeq2EnrichedIndexedSeq[A](seq:IndexedSeq[A]):EnrichedIndexedSeq[A] =
      new EnrichedIndexedSeq[A](seq)
    implicit def any2Unit[A](a:A) = new EnrichedUnit(a)
  }

  object Functions {
    implicit def function22EnrichedFunction2[A,B](f:A=>B):EnrichedFunction2[A,B] = new EnrichedFunction2(f)


  }
}
