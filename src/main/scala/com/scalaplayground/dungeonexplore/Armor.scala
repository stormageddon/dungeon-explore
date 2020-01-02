package com.scalaplayground.dungeonexplore.Armor

abstract class Armor {
  val name: String
  var id: String
  var armorBonus: Int = 0
  var isDroppable: Boolean = false
}

class Natural extends Armor {
  val name = "natural armor"
  var id = "NATURAL_ARMOR"
  armorBonus = 0
  isDroppable = false
}

class Leather extends Armor {
  val name = "leather armor"
  var id = "LEATHER_ARMOR"
  armorBonus = 1
  isDroppable = true
}

class Chain extends Armor {
  val name = "chainmail"
  var id = "CHAINMAIL"
  armorBonus = 3
  isDroppable = true
}

class StoneArmor extends Armor {
  val name = "Stone Armor"
  var id = "STONE_ARMOR"
  armorBonus = 4
  isDroppable = false
}

class PlateMail extends Armor {
  val name = "Platemail"
  var id = "PLATE_MAIL"
  armorBonus = 5
  isDroppable = false
}

class DragonScale extends Armor {
  val name = "DragonScale"
  var id = "DRAGON_SCALE"
  armorBonus = 20
  isDroppable = false
}


