package com.scalaplayground.dungeonexplore.Shrine

import com.scalaplayground.dungeonexplore.Constants.STARTING_PLAYER_HEALTH
import com.scalaplayground.dungeonexplore.{DungeonHelper, Player}
import com.scalaplayground.dungeonexplore.Position.Position

abstract class Shrine {
  val displayChar = "s"

  val dungeonHelper = new DungeonHelper
  val position: Position

  def interact(player: Player): Unit = {

  }
}

class HealthShrine(startingPosition: Position) extends Shrine {
  override val position = startingPosition

  override def interact(player: Player): Unit = {
    println("You discover a strange shrine.\nYou feel better as you look at it.")
    player.health = dungeonHelper.clamp(STARTING_PLAYER_HEALTH, 0, STARTING_PLAYER_HEALTH)
  }

}

class StrengthShrine(startingPosition: Position) extends Shrine {
  override val position = startingPosition

  override def interact(player: Player): Unit = {
    println("You discover a strange shrine.\nIt makes you feel stronger to look at.")
    player.attackBonus = player.attackBonus + 2
  }
}

class HolyShrine(startingPosition: Position) extends Shrine {
  override val position = startingPosition

  override def interact(player: Player): Unit = {
    println("You discover a strange shrine.\nYou dip your weapon in and it gleams brilliantly.")
    player.weapon.attackBonus = player.weapon.attackBonus + 2
    player.weapon.name = s"Blessed ${player.weapon.name}"
  }
}

class CursedShrine(startingPosition: Position) extends Shrine {
  override val position = startingPosition

  override def interact(player: Player): Unit = {
    println("You discover a strange shrine.\nYou feel sick looking at it.")
    player.health = player.health - 1
  }
}