package com.deweyvm.dogue.common.logging

import java.io.{FileOutputStream, File}
import com.deweyvm.dogue.common.data.Encoding
import com.deweyvm.gleany.logging.Logger
import com.deweyvm.dogue.common.Implicits._
import com.deweyvm.gleany.data.Time


class LogLevel(val marker:String)

object Log {
  def formatStackTrace(ex:Exception):String = {
    ("Failure:\nException in thread " + Thread.currentThread.getName + "\n"
    + ex.toString + '\n'
    + ex.getStackTraceString)
  }


  case object Info extends LogLevel("INFO")
  case object Warn extends LogLevel("WARN")
  case object Error extends LogLevel("ERR ")

  private var log:Option[Log] = None

  //Must be called before logging functions or they will have no effect
  def setDirectory(dir:String) {
    log = new Log(dir).some
  }

  private def checkLog() {
    if (log.isEmpty) {
      Console.err.println("Warning: log not set " + Thread.currentThread().getId)
    }
  }

  private def useLog[A](f:Log=>A) {
    checkLog()
    log foreach f
  }

  def info(string:String) {
    useLog(_.log(Info, string))
  }

  def warn(string:String) {
    useLog(_.log(Warn, string))
  }

  def error(string:String) {
    useLog(_.log(Error, string))
  }

}

class Log(dir:String) {
  Logger.attachCrasher(dir)
  val dateStr = Time.getString
  val filename = "log_%s" format dateStr
  val logfile = dir + "/" + filename


  val file:Option[FileOutputStream] = openStream(logfile)

  private def openStream(path:String):Option[FileOutputStream] = {
    val file = new File(path)
    val dir = file.getParentFile
    if (!dir.exists() && !dir.mkdirs()) {
      return None
    }
    if (!file.exists()) {
      file.createNewFile()
    }
    new FileOutputStream(file, false).some
  }

  def log(logLevel:LogLevel, string: String, stackOffset: Int = 7) {
    val callStack = Thread.currentThread().getStackTrace
    val className = callStack(stackOffset).getClassName.split("""[.]""").last.replace("$", "")
    val s = "(%s) [%s] %s: %s".format(Time.getString, logLevel.marker, className, string)
    println(s)
    file foreach {_.write((s + "\n").getBytes("UTF-8"))}
  }
}
