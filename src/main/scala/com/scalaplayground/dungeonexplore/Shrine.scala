package com.scalaplayground.dungeonexplore.Shrine

import com.scalaplayground.dungeonexplore.constants.Constants.STARTING_PLAYER_HEALTH
import com.scalaplayground.dungeonexplore.{DungeonHelper, Player}
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.Weapons.{BlessedWeaponDecorator, CursedWeaponDecorator}
import net.team2xh.scurses.Scurses

abstract class Shrine {
  val displayChar = "s"
  var isActive = true

  val position: Position
  var tileDescription: String = "You stand at the foot of a strange shrine."

  def interact(player: Player): String = {
    isActive = false
    return ""
  }

  def render(x: Int, y: Int, screen:Scurses) = {
    screen.put(x, y, displayChar)
  }
}

case class HealthShrine(startingPosition: Position) extends Shrine {
  override val position = startingPosition

  override def interact(player: Player): String = {
    var message = ""
    if (isActive) {
      super.interact(player)
      player.health = DungeonHelper.clamp(player.maxHealth, 0, player.maxHealth)
      message = "You feel healthier as you look at the shrine."
    }
    else {
      message = "The shrine's magic has been used."
    }
    message
  }
}

case class StrengthShrine(startingPosition: Position) extends Shrine {
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

case class HolyShrine(startingPosition: Position) extends Shrine {
  override val position = startingPosition

  override def interact(player: Player): String = {
    var message = ""
    if (isActive) {
      super.interact(player)
      player.weapon = new BlessedWeaponDecorator(player.weapon)
      message = "You dip your weapon in the crystal water of the shrine and it gleams brilliantly."
    }
    else {
      message = "The shrine's magic has been used"
    }
    message
  }
}

case class CursedShrine(startingPosition: Position) extends Shrine {
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