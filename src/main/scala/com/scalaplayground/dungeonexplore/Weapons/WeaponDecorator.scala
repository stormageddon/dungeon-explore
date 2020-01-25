package com.scalaplayground.dungeonexplore.Weapons

trait WeaponDecorator extends Weapon {
  val weapon: Weapon
}

class RustyWeaponDecorator(baseWeapon: Weapon) extends WeaponDecorator {
  override val weapon = baseWeapon
  override var name = s"Rusty ${weapon.name}"
  override var damage = weapon.damage
  override var id = weapon.id
  attackBonus = baseWeapon.attackBonus - 1
  isDroppable = baseWeapon.isDroppable

  override def attack: Int = {
    weapon.attack - 1
  }
}

class FineWeaponDecorator(baseWeapon: Weapon) extends WeaponDecorator {
  override val weapon = baseWeapon
  override var name = s"Fine ${weapon.name}"
  override var damage = weapon.damage
  override var id = weapon.id
  attackBonus = weapon.attackBonus + 1
  isDroppable = baseWeapon.isDroppable

  override def attack: Int = {
    return weapon.attack + 1
  }
}

class FlamingWeaponDecorator(baseWeapon: Weapon) extends WeaponDecorator {
  override val weapon = baseWeapon
  override var name = s"Flaming ${weapon.name}"
  override var damage = (weapon.damage._1 + 1, weapon.damage._2 + 1)
  override var id = weapon.id
  attackBonus = weapon.attackBonus + 1
  isDroppable = baseWeapon.isDroppable

  override def attack: Int = {
    return weapon.attack + 1
  }
}


