package com.deweyvm.dogue.common.data

class Select(self:Boolean) {
  def select[T](tthen: =>T, telse: =>T):T = if (self) tthen else telse
}
