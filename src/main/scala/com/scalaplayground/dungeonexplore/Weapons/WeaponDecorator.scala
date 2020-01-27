package com.scalaplayground.dungeonexplore.Weapons

import com.scalaplayground.dungeonexplore.Player

trait WeaponDecorator extends Weapon {
  val weapon: Weapon

  override def interact(target: Player): Unit = {
    target.appendActionMessage(s"PICKING UP ${name}")
    target.weapon = this
  }
}

class RustyWeaponDecorator(baseWeapon: Weapon) extends WeaponDecorator {
  override val weapon = baseWeapon
  override var name = s"Rusty ${weapon.name}"
  override val tileDescription: String = name
  override var damage = weapon.damage
  id = weapon.id
  attackBonus = baseWeapon.attackBonus - 1
  isDroppable = baseWeapon.isDroppable

  override def attack: Int = weapon.attack
}

class FineWeaponDecorator(baseWeapon: Weapon) extends WeaponDecorator {
  override val weapon = baseWeapon
  override var name = s"Fine ${weapon.name}"
  override val tileDescription: String = name
  override var damage = weapon.damage
  id = weapon.id
  attackBonus = weapon.attackBonus + 1
  isDroppable = baseWeapon.isDroppable

  override def attack: Int = {
    return weapon.attack + 1
  }
}

class FlamingWeaponDecorator(baseWeapon: Weapon) extends WeaponDecorator {
  override val weapon = baseWeapon
  override var name = s"Flaming ${weapon.name}"
  override val tileDescription: String = name
  override var damage = (weapon.damage._1 + 1, weapon.damage._2 + 1)
  id = weapon.id
  attackBonus = weapon.attackBonus + 1
  isDroppable = baseWeapon.isDroppable

  override def attack: Int = {
    return weapon.attack + 1
  }
}

class BlessedWeaponDecorator(baseWeapon: Weapon) extends WeaponDecorator {
  override val weapon = baseWeapon
  override var name = s"Blessed ${weapon.name}"
  override val tileDescription: String = name
  override var damage = weapon.damage
  id = weapon.id
  attackBonus = weapon.attackBonus + 2
  isDroppable = baseWeapon.isDroppable

  override def attack: Int = weapon.attack
}

class CursedWeaponDecorator(baseWeapon: Weapon) extends WeaponDecorator {
  override val weapon = baseWeapon
  override var name = s"Cursed ${weapon.name}"
  override val tileDescription: String = name
  override var damage = (weapon.damage._1 - 1, weapon.damage._2 - 1)
  id = weapon.id
  attackBonus = weapon.attackBonus - 2
  isDroppable = baseWeapon.isDroppable

  override def attack: Int = weapon.attack
}


