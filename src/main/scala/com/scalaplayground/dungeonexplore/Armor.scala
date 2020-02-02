package com.scalaplayground.dungeonexplore.Armor

import com.scalaplayground.dungeonexplore.Item.Item
import com.scalaplayground.dungeonexplore.Player
import com.scalaplayground.dungeonexplore.constants.Constants

import scala.util.Random

abstract class Armor extends Item {
  var armorBonus: Int = 0
  var isDroppable: Boolean = false
  val dropChance: Int = Constants.ARMOR_DROP_PERCENTAGE
  override val tileDescription: String = name
  override val displayChar = "!"

  override def interact(target: Player): Unit = {
    target.appendActionMessage(s"Picked up $name")
    target.armor = this
    target.inventory.add(this)
  }
}

class Natural extends Armor {
  name = "natural armor"
  id = "NATURAL_ARMOR"
  armorBonus = 0
  isDroppable = false
}

class Leather extends Armor {
  name = "leather armor"
  id = "LEATHER_ARMOR"
  armorBonus = 1
  isDroppable = true
}

class Chain extends Armor {
  name = "chainmail"
  id = "CHAINMAIL"
  armorBonus = 3
  isDroppable = true
}

class StoneArmor extends Armor {
  name = "Stone Armor"
  id = "STONE_ARMOR"
  armorBonus = 4
  isDroppable = false
}

class PlateMail extends Armor {
  name = "Platemail"
  id = "PLATE_MAIL"
  armorBonus = 5
  isDroppable = false
}

class DragonScale extends Armor {
  name = "DragonScale"
  id = "DRAGON_SCALE"
  override val dropChance = 100
  armorBonus = 20
  isDroppable = true
}

object Armor {
  def generateArmor: Armor = {
    val possibleArmor = List[Armor](
      new Leather,
      new Chain,
      new PlateMail
    )

    possibleArmor(Random.nextInt(possibleArmor.size))
  }
}


