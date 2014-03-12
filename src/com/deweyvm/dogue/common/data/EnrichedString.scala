/******************************************************************************
 * Copyright 2013, deweyvm
 *
 * This file is part of Gleany.
 *
 * Gleany is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 *
 * Gleany is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along
 * with Gleany.
 *
 * If not, see <http://www.gnu.org/licenses/>.
 *****************************************************************************/

package com.deweyvm.dogue.common.data

import com.deweyvm.dogue.common.CommonImplicits._
import com.deweyvm.dogue.common.logging.Log
import java.util.FormatFlagsConversionMismatchException

object EnrichedString {
  def test() {
    val tests = Vector(
      ("this|is|a|test", '|', 4),
      ("", '|', 1),
      ("|", '|', 2),
      ("abc|", '|', 2)
    )

    tests foreach { case (line, sep, count) =>
      val sp = line.esplit(sep)
      //printf("line to split: <%s>\n", line)
      for (s <- sp) {
        //printf("    <%s>\n", s)
      }
      //printf("%d ==? %d\n\n", sp.length, count)
      assert(sp.length == count)
    }
  }
}

class EnrichedString(self:String) {
  /**
   * Like the java string split, but a split at the end of the string leaves an additional
   * empty string in the output. See test methods for an example.
   * @param sep the separator
   * @return the strings existing around occurrences of rep in order
   */
  def esplit(sep:Char):Vector[String] = {
    val (lines, last) = self.foldLeft(Vector[String](), "") { case ((acc,  l), c) =>
      if (c == sep) {
        (acc ++ Vector(l), "")
      } else {
        (acc, l + c)
      }
    }
    lines ++ Vector(last)
  }

  def toLines(width:Int):Vector[String] =  {
    /*val grouped = self.grouped(width - 1)
    grouped.zipWithIndex.map { case (s, i) =>
      if (s.length == width - 1 && i < grouped.length - 1) {
        s + "-"
      } else {
        s
      }
    }.toVector*/
    val (last, lines) = self.foldLeft(("", Vector[String]())){
      case ((currentLine, lines), c) =>
        val added = currentLine + c
        if (added.length == width) {
          val hyphen = if (c == ' ') "" else  "-"
          ("", lines ++ Vector(added + hyphen))
        } else {
          (added, lines)
        }
    }
    lines ++ Vector(last)

  }

  def indent(width:Int):String = {
    try {
      val space = " " * width
      space + self.esplit('\n').mkString("\n" + space)
    } catch {
      case e:FormatFlagsConversionMismatchException =>
        Log.info(Log.formatStackTrace(e))
        self
    }
  }

}
