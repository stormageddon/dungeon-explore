package com.scalaplayground.dungeonexplore.Armor

abstract class Armor {
  val name: String
  var armorBonus: Int = 0
  var isDroppable: Boolean = false
}

class Natural extends Armor {
  val name = "natural armor"
  armorBonus = 0
  isDroppable = false
}

class Leather extends Armor {
  val name = "leather armor"
  armorBonus = 1
  isDroppable = true
}

class Chain extends Armor {
  val name = "chainmail"
  armorBonus = 3
  isDroppable = true
}

class PlateMail extends Armor {
  val name = "Platemail"
  armorBonus = 5
  isDroppable = false
}

class DragonScale extends Armor {
  val name = "DragonScale"
  armorBonus = 20
  isDroppable = false
}


