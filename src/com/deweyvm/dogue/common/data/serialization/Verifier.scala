package com.deweyvm.dogue.common.data.serialization

trait Verifier[T] {
  def nonEmpty(s:String) = Option(s).find(_.length > 0)
  def filterDuplicates[K](fmtString:String, ts:Map[String,Seq[K]], getName:K=>String):Either[String, Map[String,K]] = {
    val (dupes, filmap) = ts.values.foldLeft(Vector[String](), Map[String,K]()) { case ((dup, map), v) =>
      v match {
        case b1 +: b2 +: rest =>
          val appended = dup :+ (fmtString format getName(b1))
          (appended, map.updated(getName(b1), b1))
        case b1 +: rest =>
          (dup, map.updated(getName(b1), b1))
      }
    }

    if (dupes.length > 0) {
      Left(dupes.mkString("\n"))
    } else {
      Right(filmap)
    }
  }


  def verify:Either[String,T]
}
