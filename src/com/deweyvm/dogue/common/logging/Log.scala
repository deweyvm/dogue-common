package com.deweyvm.dogue.common.logging

import java.io.{PrintStream, FileOutputStream, File}
import com.deweyvm.dogue.common.data.Encoding
import com.deweyvm.dogue.common.threading.Lock
import com.deweyvm.dogue.common.Implicits._
import com.deweyvm.gleany.data.Time
import com.deweyvm.gleany.graphics.Color


class LogLevel(val marker:String, val loudness:Int) {
  def <(other:LogLevel):Boolean = {
    loudness < other.loudness
  }
  def <=(other:LogLevel):Boolean = {
    loudness <= other.loudness
  }
}


object Log {
  private val lock = new Lock
  def formatStackTrace(ex:Throwable):String = {
    ("Failure:\nException in thread " + Thread.currentThread.getName + "\n"
    + ex.toString + '\n'
    + ex.getStackTraceString)
  }

  case object All extends LogLevel("ALL ", Int.MaxValue)
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
    lock.foreach[Unit](_ =>
      log foreach f
    )()
  }

  def all(s:String) {
    useLog(_.log(All, s))
  }

  def verbose(s:String) {
    useLog(_.log(Verbose, s))
  }

  def info(s:String) {
    useLog(_.log(Info, s))
  }

  def warn(s:String) {
    useLog(_.log(Warn, s))
  }

  def error(s:String) {
    useLog(_.log(Error, s))
  }

  def flush() {
    useLog(_.flush())
  }

  private def attachCrasher(file:FileOutputStream) {
    Thread.setDefaultUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler {
      def uncaughtException(t: Thread, e: Throwable) {
        try {
          Log.flush()
        }
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

  def testLog(className:String) {
    Log.info(className)
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

  /**
   * The jvm mangles scala class names in an implementation specific way. Here
   * we at least try to account for the ones we know of.
   * @param name
   */
  private def transformName(name:String):String = {
    val maxNameLength = 16
    val className = name.split("\\$", 2)(0).split("anonfun", 2)(0)
    val toFormat =
      if (className.length > maxNameLength) {
        className.substring(0, maxNameLength - 3) + "..."
      } else {
        className
      }
    val formatString = "%" + maxNameLength + "s"
    formatString format toFormat
  }

  def flush() {
    file foreach {_.flush()}
  }

  def log(level:LogLevel, string: String, stackOffset: Int = 11) {
    try {
      if (logLevel < level) {

        return
      }
      val callStack = Thread.currentThread().getStackTrace
      val className = callStack(stackOffset).getClassName.split("""[.]""").last
      val s = "(%s) [%s] %s: %s".format(Time.getString, level.marker, transformName(className), string).replace("\0", "\\0")
      println(s)
      file foreach {_.write((s + "\n").getBytes("UTF-8"))}
    } catch {
      case t:Throwable =>
        println(Log.formatStackTrace(t))
    }
  }
}
