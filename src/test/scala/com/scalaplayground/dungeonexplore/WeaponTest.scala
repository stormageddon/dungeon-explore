package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Weapons._
import org.mockito.MockitoSugar
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers


class WeaponTest
  extends AnyFlatSpec
  with Matchers
  with MockitoSugar {
  behavior of "Weapon"

  it should "create a weapon" in {
    val weapon = new Dagger

    weapon.name mustBe "Dagger"
    weapon.id mustBe "DAGGER"

  }

  it should "combine decorator damage on attack" in {
    val daggerSpy = mock[Dagger]
    when(daggerSpy.attack(None, None)).thenReturn(100)

    val weapon: Weapon = RustyWeaponDecorator(daggerSpy)

    val result = weapon.attack()

    result mustBe 99
  }

  it should "create a RUSTY weapon" in {
    val weapon: Weapon = RustyWeaponDecorator(new Dagger)

    weapon.name mustBe "Rusty Dagger"
    weapon.id mustBe "RUSTY_DAGGER"
    weapon.attackBonus mustBe -1
  }

  it should "create a FINE weapon" in {
    val weapon: Weapon = FineWeaponDecorator(new GreatAxe)

    weapon.name mustBe "Fine Great Axe"
    weapon.id mustBe "FINE_GREAT_AXE"
    weapon.damage mustBe (1,6)
    weapon.attackBonus mustBe 1
  }

  it should "allow multiple decorations on a single weapon" in {
    val weapon: Weapon = FlamingWeaponDecorator(new FineWeaponDecorator(new Spear))

    weapon.name mustBe "Flaming Fine Spear"
    weapon.id mustBe "FLAMING_FINE_SPEAR"
    weapon.damage mustBe (2,4)
    weapon.attackBonus mustBe +2
  }

  it should "pick up a weapon" in {
    val fineWeapon: Weapon = FineWeaponDecorator(new ShortSword)
    val daggerWeapon = new Dagger
    val player: Player = new Player("Test Player", "Klass", "Race")

    player.inventory.getItems.size mustBe 1 // Dagger

    fineWeapon.interact(player)

    player.weapon mustEqual(daggerWeapon)
    player.inventory.getItems.size mustBe 2 // fineWeapon is picked up
  }

  it should "use the base weapons droppable status" in {
    val weapon = FineWeaponDecorator(new ShortSword)
    weapon.isDroppable mustBe true
  }

  it should "set weapon to unidentified and enchanted when decorated" in {
    var weapon: Weapon = new Dagger
    weapon.enchanted mustBe false
    weapon.identified mustBe true

    weapon = FlamingWeaponDecorator(weapon)
    weapon.enchanted mustBe true
    weapon.identified mustBe false
  }
}
