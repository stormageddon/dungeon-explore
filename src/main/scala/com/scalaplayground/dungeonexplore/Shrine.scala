package com.scalaplayground.dungeonexplore.Shrine

import com.scalaplayground.dungeonexplore.Constants.STARTING_PLAYER_HEALTH
import com.scalaplayground.dungeonexplore.{DungeonHelper, Player}
import com.scalaplayground.dungeonexplore.Position.Position

abstract class Shrine {
  val displayChar = "s"
  var isActive = true

  val dungeonHelper = new DungeonHelper
  val position: Position
  var tileDescription: String = "You stand at the foot of a strange shrine."

  def interact(player: Player): String = {
    isActive = false
    return ""
  }

  def render = {
    print(displayChar)
  }
}

class HealthShrine(startingPosition: Position) extends Shrine {
  override val position = startingPosition

  override def interact(player: Player): String = {
    var message = ""
    if (isActive) {
      super.interact(player)
      player.health = dungeonHelper.clamp(STARTING_PLAYER_HEALTH, 0, STARTING_PLAYER_HEALTH)
      message = "You feel better as you look at the shrine."
    }
    else {
      message = "The shrine's magic has been used."
    }
    message
  }
}

class StrengthShrine(startingPosition: Position) extends Shrine {
  override val position = startingPosition

  override def interact(player: Player): String = {
    var message = ""

    if (isActive) {
      super.interact(player)
      player.attackBonus = player.attackBonus + 2
      message = "It makes you feel stronger to look at the shrine."
    }
    else {
      message = "The shrine's magic has been used"
    }
    message
  }
}

class HolyShrine(startingPosition: Position) extends Shrine {
  override val position = startingPosition

  override def interact(player: Player): String = {
    var message = ""
    if (isActive) {
      super.interact(player)
      player.weapon.attackBonus = player.weapon.attackBonus + 2
      player.weapon.name = s"Blessed ${player.weapon.name}"
      message = "You dip your weapon in the crystal water of the shrine and it gleams brilliantly."
    }
    else {
      message = "The shrine's magic has been used"
    }
    message
  }
}

class CursedShrine(startingPosition: Position) extends Shrine {
  override val position = startingPosition

  override def interact(player: Player): String = {
    var message = ""
    if (isActive) {
      super.interact(player)
      player.health = player.health - 1
      message = "You feel sick looking at the shrine."
    }
    else {
      message = "The shrine's magic has been used"
    }
    message
  }
}