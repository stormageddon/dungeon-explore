package com.scalaplayground.dungeonexplore

object DungeonHelper {
  def clamp(num: Int, min: Int, max: Int): Int = {
    if (num < min) min else if (num > max) max else num
  }

  def padGameObjectChar(goChar:String): String = {
    s" ${goChar} "
  }
}
