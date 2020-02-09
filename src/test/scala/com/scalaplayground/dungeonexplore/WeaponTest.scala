package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Weapons._
import org.mockito.Mockito.when
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{FlatSpec, Matchers}

class WeaponTest
  extends FlatSpec
  with Matchers
  with MockitoSugar {
  behavior of "Weapon"

  it should "create a weapon" in {
    val weapon = new Dagger

    weapon.name shouldBe "Dagger"
    weapon.id shouldBe "DAGGER"

  }

  it should "combine decorator damage on attack" in {
    val daggerSpy = mock[Dagger]
    when(daggerSpy.attack(None, None)).thenReturn(100)

    val weapon: Weapon = new RustyWeaponDecorator(daggerSpy)

    val result = weapon.attack()

    result shouldBe 99
  }

  it should "create a RUSTY weapon" in {
    val weapon: Weapon = new RustyWeaponDecorator(new Dagger)

    weapon.name should be ("Rusty Dagger")
    weapon.id should be ("RUSTY_DAGGER")
    weapon.attackBonus should be (-1)
  }

  it should "create a FINE weapon" in {
    val weapon: Weapon = new FineWeaponDecorator(new GreatAxe)

    weapon.name should be ("Fine Great Axe")
    weapon.id should be ("FINE_GREAT_AXE")
    weapon.damage should be ((1,6))
    weapon.attackBonus should be (1)
  }

  it should "allow multiple decorations on a single weapon" in {
    val weapon: Weapon = new FlamingWeaponDecorator(new FineWeaponDecorator(new Spear))

    weapon.name should be ("Flaming Fine Spear")
    weapon.id should be ("FLAMING_FINE_SPEAR")
    weapon.damage should be ((2,4))
    weapon.attackBonus should be (+2)
  }

  it should "pick up a weapon" in {
    val fineWeapon: Weapon = new FineWeaponDecorator(new ShortSword)
    val daggerWeapon = new Dagger
    val player: Player = new Player("Test Player", "Klass", "Race")

    player.weapon = daggerWeapon

    player.inventory.getItems.size shouldBe 2 // Dagger and potion

    fineWeapon.interact(player)

    player.weapon shouldEqual(daggerWeapon)
    player.inventory.getItems.size shouldBe 3 // fineWeapon is picked up
  }

  it should "use the base weapons droppable status" in {
    val weapon = new FineWeaponDecorator(new ShortSword)
    weapon.isDroppable shouldBe true
  }

  it should "set weapon to unidentified and enchanted when decorated" in {
    var weapon: Weapon = new Dagger
    weapon.enchanted shouldBe false
    weapon.identified shouldBe true

    weapon = new FlamingWeaponDecorator(weapon)
    weapon.enchanted shouldBe true
    weapon.identified shouldBe false
  }
}
