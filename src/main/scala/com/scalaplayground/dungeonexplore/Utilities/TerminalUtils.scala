package com.scalaplayground.dungeonexplore.Utilities

object TerminalUtils {

  private def getShell: String = {
    if (System.getProperty("os.name").toLowerCase.contains("window")) {
      "bash"
    }
    else {
      "/bin/sh"
    }
  }

    def setToRaw = Array[String](getShell, "-c", "stty raw </dev/tty")
    def setToCooked = Array[String](getShell, "-c", "stty cooked </dev/tty")
}
