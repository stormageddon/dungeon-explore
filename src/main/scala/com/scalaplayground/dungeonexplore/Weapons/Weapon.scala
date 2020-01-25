package com.scalaplayground.dungeonexplore.Weapons


import com.scalaplayground.dungeonexplore.Item.Interactable
import com.scalaplayground.dungeonexplore.Player
import com.scalaplayground.dungeonexplore.constants.Constants

import scala.util.Random

abstract class Weapon extends Interactable {
  var name: String
  var damage: (Int, Int)
  var id: String
  var attackBonus: Int = 0
  var isDroppable: Boolean = false
  val dropChance = Constants.WEAPON_DROP_PERCENTAGE

  def attack: Int = {
    Random.nextInt(damage._2) + damage._1
  }

  def getAttackText: String = {
    s"swings at you with their ${name}"
  }

  override def interact(target: Player): Unit = {
    target.appendActionMessage(s"DROPPING ${name}")
    target.weapon = this
  }
}

class Dagger extends Weapon {
  var name = "Dagger"
  var damage = (1,2)
  var id = "DAGGER"
  isDroppable = true
}

class ShortSword extends Weapon {
  var name = "Short sword"
  var damage = (1,4)
  var id = "SHORT_SWORD"
  isDroppable = true
}

class GreatAxe extends Weapon {
  var name = "Great Axe"
  var damage = (1,6)
  var id = "GREAT_AXE"
  isDroppable = true
}

class Claws extends Weapon {
  var name = "claws"
  var damage = (1,4)
  var id = "CLAWS"
  isDroppable = false
}

class NightBlade extends Weapon {
  var name = "Night Blade"
  var damage = (4,10)
  var id = "NIGHT_BLADE"
  override val dropChance = 100
  isDroppable = true
}

class Spear extends Weapon {
  var name = "Spear"
  var damage = (1,3)
  var id = "SPEAR"
  isDroppable = true
}