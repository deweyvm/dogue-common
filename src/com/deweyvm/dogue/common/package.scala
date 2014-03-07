package com.deweyvm.dogue

import java.net.Socket
import scala.language.implicitConversions
import com.deweyvm.dogue.common.data._
import com.deweyvm.dogue.common.io.{EnrichedInputStream, EnrichedOutputStream, EnrichedSocket}
import java.io.{OutputStream, InputStream}

package object common {
  def id[T](x:T) = x
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
    implicit def any2Printable[A](a:A) = new Printable[A](a)
    implicit def bool2Select[A](b:Boolean) = new Select(b)
    implicit def val2Scalar[A](t:A)(implicit n:Numeric[A])  = new Scalar(t)
    implicit def any2Range[A](self:A) = new RangeCompanion(self)

    implicit class Meters(val d:Double) extends AnyVal {
      def m:Meters = this
      def f = d.toFloat
      def >(other:Meters) = d > other.d
      def >=(other:Meters) = d >= other.d
      def <(other:Meters) = d < other.d
      def <=(other:Meters) = d <= other.d
      def unary_- = Meters(-d)
      override def toString = "%.2fm" format d
    }

    implicit val metersOrdered = new Ordering[Meters] {
      def compare(m1:Meters, m2:Meters) = m1.d.compare(m2.d)
    }

    implicit class Pressure(val d:Double) extends AnyVal {
      def atm:Pressure = this
    }

  }

  object Functions {
    implicit def function22EnrichedFunction2[A,B](f:A=>B):EnrichedFunction2[A,B] = new EnrichedFunction2(f)


  }
}
