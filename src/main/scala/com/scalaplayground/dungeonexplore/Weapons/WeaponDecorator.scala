package com.scalaplayground.dungeonexplore.Weapons

import com.scalaplayground.dungeonexplore.Monster.{CharacterObject, Monster}
import com.scalaplayground.dungeonexplore.{DungeonHelper, Player}

import scala.util.Random

trait WeaponDecorator extends Weapon {
  val weapon: Weapon

  override def interact(target: Player): Unit = {
    target.appendActionMessage(s"Picked up ${name}")
    target.weapon = this
    target.inventory.add(this)
  }
}

class RustyWeaponDecorator(baseWeapon: Weapon) extends WeaponDecorator {
  override val weapon = baseWeapon
  name = s"Rusty ${weapon.name}"
  override val tileDescription: String = name
  override var damage = weapon.damage
  id = s"RUSTY_${weapon.id}"
  attackBonus = baseWeapon.attackBonus - 1
  isDroppable = baseWeapon.isDroppable

  override def attack(target: Option[CharacterObject] = None, wielder: Option[CharacterObject] = None): Int = weapon.attack(target, wielder)
}

class FineWeaponDecorator(baseWeapon: Weapon) extends WeaponDecorator {
  override val weapon = baseWeapon
  name = s"Fine ${weapon.name}"
  override val tileDescription: String = name
  override var damage = weapon.damage
  id = s"FINE_${weapon.id}"
  attackBonus = weapon.attackBonus + 1
  isDroppable = baseWeapon.isDroppable

  override def attack(target: Option[CharacterObject] = None, wielder: Option[CharacterObject] = None): Int = {
    return weapon.attack() + 1
  }
}

class FlamingWeaponDecorator(baseWeapon: Weapon) extends WeaponDecorator {
  override val weapon = baseWeapon
  name = s"Flaming ${weapon.name}"
  override val tileDescription: String = name
  override var damage = (weapon.damage._1 + 1, weapon.damage._2 + 1)
  id = s"FLAMING_${weapon.id}"
  attackBonus = weapon.attackBonus + 1
  isDroppable = baseWeapon.isDroppable
  identified = false

  override def attack(target: Option[CharacterObject] = None, wielder: Option[CharacterObject] = None): Int = {
    return weapon.attack() + 1
  }
}

class BlessedWeaponDecorator(baseWeapon: Weapon) extends WeaponDecorator {
  override val weapon = baseWeapon
  name = s"Blessed ${weapon.name}"
  override val tileDescription: String = name
  override var damage = weapon.damage
  id = s"BLESSED_${weapon.id}"
  attackBonus = weapon.attackBonus + 2
  isDroppable = baseWeapon.isDroppable
  identified = false

  override def attack(target: Option[CharacterObject] = None,
                      wielder: Option[CharacterObject] = None): Int = {
    return weapon.attack()
  }
}

class CursedWeaponDecorator(baseWeapon: Weapon) extends WeaponDecorator {
  override val weapon = baseWeapon
  name = s"Cursed ${weapon.name}"
  override val tileDescription: String = name
  override var damage = (weapon.damage._1 - 1, weapon.damage._2 - 1)
  id = s"CURSED_${weapon.id}"
  attackBonus = weapon.attackBonus - 2
  isDroppable = baseWeapon.isDroppable
  identified = false

  override def attack(target: Option[CharacterObject] = None,
                      wielder: Option[CharacterObject] = None): Int = {
    weapon.attack()
  }
}

class VampiricDecorator(baseWeapon: Weapon) extends WeaponDecorator {
  override val weapon = baseWeapon
  name = s"Vampiric ${weapon.name}"
  override val tileDescription: String = name
  override var damage = weapon.damage
  id = s"VAMPIRIC_${weapon.id}"
  attackBonus = weapon.attackBonus + 2
  isDroppable = baseWeapon.isDroppable
  identified = false

  override def attack(target: Option[CharacterObject] = None,
                      wielder: Option[CharacterObject] = None): Int = {
    val damage = weapon.attack()
    val t = target.orNull
    val w = wielder.orNull

    if (t != null && w != null && Random.nextInt(100) < 20) {
      w.asInstanceOf[Player].appendActionMessage(s"${w.asInstanceOf[Player].name} saps $damage points of health from ${t.asInstanceOf[Monster].name}! ")
      w.health = DungeonHelper.clamp(w.health + damage, 0, w.maxHealth)
    }

    damage
  }

}


