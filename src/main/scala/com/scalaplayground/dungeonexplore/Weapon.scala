package com.scalaplayground.dungeonexplore.Weapon

import com.scalaplayground.dungeonexplore.constants.Constants

import scala.util.Random

abstract class Weapon {
  var name: String
  var damage: (Int, Int)
  var id: String
  var attackBonus: Int = 0
  var isDroppable: Boolean = false
  val dropChance = Constants.WEAPON_DROP_PERCENTAGE
  //var attackText = s"swings at you with their ${this.name}"

  def attack: Int = {
    val damageDealt = Random.nextInt(damage._2) + damage._1
    damageDealt
  }

  def getAttackText: String = {
    s"swings at you with their ${name}"
  }
}

class RustyDagger extends Weapon {
  var name = "Rusty dagger"
  var damage = (1,2)
  var id = "RUSTY_DAGGER"
  isDroppable = true
  attackBonus = -1
}

class Dagger extends Weapon {
  var name = "Dagger"
  var damage = (1,2)
  var id = "DAGGER"
  isDroppable = true
}

class FineDagger extends Weapon {
  var name = "Fine Dagger"
  var damage = (1,3)
  var id = "FINE_DAGGER"
  isDroppable = true
  attackBonus = 1
}

class RustyShortSword extends Weapon {
  var name = "Rusty short sword"
  var damage = (1,4)
  var id = "RUSTY_SHORT_SWORD"
  isDroppable = true
  attackBonus = -1
}

class ShortSword extends Weapon {
  var name = "Short sword"
  var damage = (1,4)
  var id = "SHORT_SWORD"
  isDroppable = true
}

class FineShortSword extends Weapon {
  var name = "Fine short sword"
  var damage = (1,5)
  var id = "FINE_SHORT_SWORD"
  isDroppable = true
  attackBonus = 1
}

class RustyGreatAxe extends Weapon {
  var name = "Rusty Great Axe"
  var damage = (1, 6)
  var id = "RUSTY_GREAT_AXE"
  isDroppable = true
  attackBonus = -1
}


class GreatAxe extends Weapon {
  var name = "Great Axe"
  var damage = (1,6)
  var id = "GREAT_AXE"
  isDroppable = true
}

class FineGreatAxe extends Weapon {
  var name = "Fine Great Axe"
  var damage = (1,7)
  var id = "FINE_GREAT_AXE"
  isDroppable = true
  attackBonus = 1
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
  var name = "spear"
  var damage = (1,3)
  var id = "SPEAR"
  isDroppable = true
}