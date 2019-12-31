package com.scalaplayground.dungeonexplore.Item

import com.scalaplayground.dungeonexplore.Player
import com.scalaplayground.dungeonexplore.Position.Position

class Item(startingPos: Position, dispChar: String) {
  val position: Position = startingPos
  val displayChar: String = dispChar

  def render(): Unit = {
    print(displayChar)
  }

  def interact(player: Player): Unit = {
    player.numPotions = player.numPotions + 1
    println(s"You picked up a potion!")
  }
}
