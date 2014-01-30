package com.deweyvm.dogue.common.io

import java.net.Socket

object DogueSocket {
  def create(port:Int, address:String)
  def create(port:Int) = new DogueSocket
}

class DogueSocket(socket:Socket) {

}
