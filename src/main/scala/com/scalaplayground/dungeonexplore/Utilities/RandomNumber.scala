package com.scalaplayground.dungeonexplore.Utilities

import scala.util.Random

class RandomNumber {
  def nextInt(max: Int): Int = {
    return Random.nextInt(max)
  }
}