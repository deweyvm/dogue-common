package com.deweyvm.dogue.common.data

import com.deweyvm.dogue.common.CommonImplicits
import CommonImplicits._

class Select(self:Boolean) {
  def select[T](tthen: =>T, telse: =>T):T = if (self) tthen else telse
  def partial[T](tthen: =>T):Option[T] = if (self) tthen.some else None
}
