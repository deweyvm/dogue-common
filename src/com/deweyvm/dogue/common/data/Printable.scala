package com.deweyvm.dogue.common.data

class Printable[T](a:T) {
  def print():T = {
    System.out.print(a.toString)
    a
  }
  def println():T = {
    System.out.println(a)
    a
  }
}
