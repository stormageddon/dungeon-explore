package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Item.Item
import com.scalaplayground.dungeonexplore.Position.Position

import scala.util.Random

sealed trait Potion extends Item {
  override val displayChar: String = "!"
  def description: String = if (identified) name else s"swirling ${color} potion"
  def consume(target: Player): String
  val color: String

  override def interact(target: Player): Unit = {
    target.appendActionMessage(s"Picked up ${this.description}")
    target.inventory.add(this)
  }
}

class HealthPotion(pos:Position) extends Potion {
  name = "Healing Potion"
  id = "POTION_HEALTH"
  override val color = "red"
  override def description: String = if (HealthPotion.isIdentified) name else s"swirling ${color} potion"
  position = pos

  def consume(target: Player): String = {
    val healthRegained = Random.nextInt(6) + 1
    target.health = DungeonHelper.clamp(target.health + healthRegained, 0, target.maxHealth)
    s"You quickly quaff the potion, regaining ${healthRegained} health."
  }
}

object HealthPotion {
  var isIdentified = false
}

class HardenedArmorPotion(pos:Position) extends Potion {
  name = "Stone Skin Potion"
  id = "POTION_STONE_SKIN"
  position = pos
  override val color = "grey"
  override def description: String = if (HardenedArmorPotion.isIdentified) name else s"swirling ${color} potion"

  def consume(target: Player): String = {
    target.armorClass = target.armorClass + 1
    "Your skin hardens like rock"
  }
}

object HardenedArmorPotion {
  var isIdentified = false
}

class PoisonPotion(pos:Position) extends Potion {
  name = "Poison"
  id = "POTION_POISON"
  position = pos
  override val color = "green"
  override def description: String = if (PoisonPotion.isIdentified) name else s"swirling ${color} potion"

  def consume(target: Player): String = {
    target.conditions = target.conditions :+ Poisoned(target)
    "You feel sick"
  }
}

object PoisonPotion {
  var isIdentified = false
}

class TelepathyPotion(pos:Position) extends Potion {
  name = "Telepathy Potion"
  id = "POTION_TELEPATHY"
  position = pos
  override val color = "blue"
  override def description: String = if (TelepathyPotion.isIdentified) name else s"swirling ${color} potion"

  def consume(target: Player): String = {
    target.conditions = target.conditions :+ Telepathic(target)
    "You can suddenly hear the thoughts of everything on the floor!"
  }
}

object TelepathyPotion {
  var isIdentified = false
}
