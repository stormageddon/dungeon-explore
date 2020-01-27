package com.scalaplayground.dungeonexplore.Weapons


import com.scalaplayground.dungeonexplore.Item.Item
import com.scalaplayground.dungeonexplore.Player
import com.scalaplayground.dungeonexplore.constants.Constants
import net.team2xh.scurses.Scurses

import scala.util.Random

abstract class Weapon extends Item {
  var name: String
  var damage: (Int, Int)
  var attackBonus: Int = 0
  var isDroppable: Boolean = false
  val dropChance = Constants.WEAPON_DROP_PERCENTAGE
  override val tileDescription: String = name
  override val displayChar = "!"

  def attack: Int = {
    Random.nextInt(damage._2) + damage._1
  }

  def getAttackText: String = {
    s"swings at you with their ${name}"
  }

  override def render(x: Int, y: Int, screen: Scurses): Unit = {
    screen.put(x, y, displayChar)
  }

  override def interact(target: Player): Unit = {
    target.appendActionMessage(s"PICKING UP ${name}")
    target.weapon = this
  }
}

class Dagger extends Weapon {
  var name = "Dagger"
  override val tileDescription: String = name
  var damage = (1,2)

  id = "DAGGER"
  isDroppable = true
}

class ShortSword extends Weapon {
  var name = "Short sword"
  override val tileDescription: String = name
  var damage = (1,4)
  id = "SHORT_SWORD"
  isDroppable = true
}

class GreatAxe extends Weapon {
  var name = "Great Axe"
  override val tileDescription: String = name
  var damage = (1,6)
  id = "GREAT_AXE"
  isDroppable = true
}

class Claws extends Weapon {
  var name = "claws"
  var damage = (1,4)
  id = "CLAWS"
  isDroppable = false
}

class NightBlade extends Weapon {
  var name = "Night Blade"
  var damage = (4,10)
  override val tileDescription: String = name
  id = "NIGHT_BLADE"
  override val dropChance = 100
  isDroppable = true
}

class Spear extends Weapon {
  var name = "Spear"
  override val tileDescription: String = name
  var damage = (1,3)
  id = "SPEAR"
  isDroppable = true
}