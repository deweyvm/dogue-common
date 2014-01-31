package com.deweyvm.dogue.common.logging

import java.io.{PrintStream, FileOutputStream, File}
import com.deweyvm.dogue.common.data.Encoding
import com.deweyvm.gleany.logging.Logger
import com.deweyvm.dogue.common.Implicits._
import com.deweyvm.gleany.data.Time


class LogLevel(val marker:String, val loudness:Int) {
  def <(other:LogLevel):Boolean = {
    loudness < other.loudness
  }
  def <=(other:LogLevel):Boolean = {
    loudness <= other.loudness
  }
}

object Log {
  private val lock = new scala.concurrent.Lock
  def formatStackTrace(ex:Throwable):String = {
    ("Failure:\nException in thread " + Thread.currentThread.getName + "\n"
    + ex.toString + '\n'
    + ex.getStackTraceString)
  }

  case object Verbose extends LogLevel("LOUD", 4)
  case object Info extends LogLevel("INFO", 3)
  case object Warn extends LogLevel("WARN", 2)
  case object Error extends LogLevel("ERR ", 1)
  case object Silent extends LogLevel("NONE", 0)
  private var log:Option[Log] = None

  //Must be called before logging functions or they will have no effect
  def initLog(dir:String, level:LogLevel) {
    log = new Log(dir, level).some
  }

  private def checkLog() {
    if (log.isEmpty) {
      println("Warning: log not set " + Thread.currentThread().getId)
    }
  }

  private def useLog[A](f:Log=>A) {
    checkLog()
    lock.acquire()
    log foreach f
    lock.release()
  }

  def verbose(string:String) {
    useLog(_.log(Verbose, string))
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

  private def attachCrasher(file:FileOutputStream) {
    Thread.setDefaultUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler {
      def uncaughtException(t: Thread, e: Throwable) {
        writeCrashToFile(file, e)
        //let the program crash anyway
        System.err.print(formatStackTrace(e))
      }
    })
  }

  private def writeCrashToFile(file:FileOutputStream, e:Throwable) {
    try {
      file.write(Encoding.toBytes(Log.formatStackTrace(e)))
      file.close()
    } catch {
      case t: Throwable =>
        System.err.print(formatStackTrace(t))

    }
  }

}

class Log(dir:String, logLevel:LogLevel) {
  import Log._
  val dateStr = Time.getString
  val filename = "log_%s" format dateStr
  val logfile = dir + "/" + filename

  val file:Option[FileOutputStream] = openStream(logfile)
  file foreach { stream =>
    attachCrasher(stream)
  }

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

  def log(level:LogLevel, string: String, stackOffset: Int = 7) {
    try {
      if (logLevel < level) {
        println("%d < %d" format (logLevel.loudness, level.loudness))
        return
      }
      val callStack = Thread.currentThread().getStackTrace
      val className = callStack(stackOffset).getClassName.split("""[.]""").last.replace("$", "")
      val s = "(%s) [%s] %s: %s".format(Time.getString, level.marker, className, string)
      println(s.replace("\0", "\\0"))
      file foreach {_.write((s + "\n").getBytes("UTF-8"))}
    } catch {
      case t:Throwable =>
        println(Log.formatStackTrace(t))
    }
  }
}
