package com.deweyvm.dogue.common.reflect

object Reflection {
  def getEnum[K, T](t:T, cls:Class[_ <: T], name:String, exclude:String => Boolean):Vector[K] = {
    val methods = cls.getDeclaredMethods
    methods.filter { m =>
      !exclude(m.getName) &&
        m.getReturnType.getSimpleName == name &&
        m.getParameterTypes.length == 0
    }.map { m =>
      m.invoke(t).asInstanceOf[K]
    }.toVector
  }
}
