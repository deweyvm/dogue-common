package com.deweyvm.dogue.common.threading


abstract class Task {
  def execute():Unit
  final def start():Task = {
    ThreadManager.spawn(this)
    this
  }
}
