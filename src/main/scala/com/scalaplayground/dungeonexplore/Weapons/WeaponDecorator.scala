package com.scalaplayground.dungeonexplore.Weapons

import com.scalaplayground.dungeonexplore.Player

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

  override def attack: Int = weapon.attack
}

class FineWeaponDecorator(baseWeapon: Weapon) extends WeaponDecorator {
  override val weapon = baseWeapon
  name = s"Fine ${weapon.name}"
  override val tileDescription: String = name
  override var damage = weapon.damage
  id = s"FINE_${weapon.id}"
  attackBonus = weapon.attackBonus + 1
  isDroppable = baseWeapon.isDroppable

  override def attack: Int = {
    return weapon.attack + 1
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

  override def attack: Int = {
    return weapon.attack + 1
  }
}

class BlessedWeaponDecorator(baseWeapon: Weapon) extends WeaponDecorator {
  override val weapon = baseWeapon
  name = s"Blessed ${weapon.name}"
  override val tileDescription: String = name
  override var damage = weapon.damage
  id = s"BLESSED${weapon.id}"
  attackBonus = weapon.attackBonus + 2
  isDroppable = baseWeapon.isDroppable
  identified = false

  override def attack: Int = weapon.attack
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

  override def attack: Int = weapon.attack
}


