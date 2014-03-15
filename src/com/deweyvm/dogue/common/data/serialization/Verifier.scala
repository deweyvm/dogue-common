package com.deweyvm.dogue.common.data.serialization

trait Verifier[T] {
  def verify:Either[String,T]
}
