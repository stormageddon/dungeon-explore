package com.scalaplayground.dungeonexplore.Weapon

import scala.util.Random

abstract class Weapon {
  var name: String
  var damage: (Int, Int)
  var attackBonus: Int = 0
  var isDroppable: Boolean = false

  def attack: Int = {
    val damageDealt = Random.nextInt(damage._2) + damage._1
    println(s"You stab with your ${name} dealing ${damageDealt} damage!")
    damageDealt
  }
}

class RustyDagger extends Weapon {
  var name = "Rusty dagger"
  var damage = (1,2)
  isDroppable = true
  attackBonus = -1
}

class Dagger extends Weapon {
  var name = "Dagger"
  var damage = (1,2)
  isDroppable = true
}

class FineDagger extends Weapon {
  var name = "Fine Dagger"
  var damage = (1,3)
  isDroppable = true
  attackBonus = 1
}

class RustyShortSword extends Weapon {
  var name = "Rusty short sword"
  var damage = (1,4)
  isDroppable = true
  attackBonus = -1
}

class ShortSword extends Weapon {
  var name = "Short sword"
  var damage = (1,4)
  isDroppable = true
}

class FineShortSword extends Weapon {
  var name = "Fine short sword"
  var damage = (1,5)
  isDroppable = true
  attackBonus = 1
}

class RustyGreatAxe extends Weapon {
  var name = "Rusty Great Axe"
  var damage = (1, 6)
  isDroppable = true
  attackBonus = -1
}


class GreatAxe extends Weapon {
  var name = "Great Axe"
  var damage = (1,6)
  isDroppable = true
}

class FineGreatAxe extends Weapon {
  var name = "Fine Great Axe"
  var damage = (1,7)
  isDroppable = true
  attackBonus = 1
}

class Claws extends Weapon {
  var name = "claws"
  var damage = (1,4)
  isDroppable = false
}

class NightBlade extends Weapon {
  var name = "Night Blade"
  var damage = (2,7)
  isDroppable = true
}

class Spear extends Weapon {
  var name = "spear"
  var damage = (1,3)
  isDroppable = true
}