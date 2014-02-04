package com.deweyvm.dogue.common.data

import java.security.{SecureRandom, MessageDigest}

/**
 * This class is NOT secure and is for learning purposes only!
 */
object Crypto {
  private val rng = new SecureRandom()
  private val sha256 = MessageDigest.getInstance("SHA-256")

  private def hash64(string:String):String = {
    sha256.update(Encoding.toBytes(string))
    val result = javax.xml.bind.DatatypeConverter.printBase64Binary(sha256.digest())
    sha256.reset()
    result
  }

  def comparePassword(password:String, salt:String, hash:String):Boolean = {
    val computedHash = getHash(password, salt)
    hash == computedHash
  }

  private def getHash(password:String, salt:String):String = {
    (0 until 4).foldLeft[String](password+salt) { case (acc:String, _) => hash64(acc + password + salt) }
  }

  def generatePassword:(String, String, String) = {
    def r = rng.nextInt
    val password = "%08x%08x%08x%08x" format (r,r,r,r)
    val salt = "%08x" format r
    val hash:String = getHash(password, salt)
    (password, salt, hash)
  }
}
