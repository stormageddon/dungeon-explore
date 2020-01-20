package com.scalaplayground.dungeonexplore.Shrine

import com.scalaplayground.dungeonexplore.constants.Constants.STARTING_PLAYER_HEALTH
import com.scalaplayground.dungeonexplore.{DungeonHelper, Player}
import com.scalaplayground.dungeonexplore.Position.Position
import net.team2xh.scurses.Scurses

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

  def render(x: Int, y: Int, screen:Scurses) = {
    //print(dungeonHelper.padGameObjectChar(displayChar))
    screen.put(x, y, displayChar)
  }
}

class HealthShrine(startingPosition: Position) extends Shrine {
  override val position = startingPosition

  override def interact(player: Player): String = {
    var message = ""
    if (isActive) {
      super.interact(player)
      player.health = dungeonHelper.clamp(STARTING_PLAYER_HEALTH, 0, STARTING_PLAYER_HEALTH)
      message = "You feel healthier as you look at the shrine.\n"
    }
    else {
      message = "The shrine's magic has been used.\n"
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
      message = "It makes you feel stronger to look at the shrine.\n"
    }
    else {
      message = "The shrine's magic has been used\n"
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
      message = "You dip your weapon in the crystal water of the shrine and it gleams brilliantly.\n"
    }
    else {
      message = "The shrine's magic has been used\n"
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
      message = "You feel sick looking at the shrine.\n"
    }
    else {
      message = "The shrine's magic has been used\n"
    }
    message
  }
}