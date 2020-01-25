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
    when(daggerSpy.attack).thenReturn(100)

    val weapon: Weapon = new RustyWeaponDecorator(daggerSpy)

    val result = weapon.attack

    result shouldBe 99
  }

  it should "create a RUSTY weapon" in {
    val weapon: Weapon = new RustyWeaponDecorator(new Dagger)

    weapon.name should be ("Rusty Dagger")
    weapon.id should be ("DAGGER")
    weapon.attackBonus should be (-1)
  }

  it should "create a FINE weapon" in {
    val weapon: Weapon = new FineWeaponDecorator(new GreatAxe)

    weapon.name should be ("Fine Great Axe")
    weapon.id should be ("GREAT_AXE")
    weapon.damage should be ((1,6))
    weapon.attackBonus should be (+1)
  }

  it should "allow multiple decorations on a single weapon" in {
    val weapon: Weapon = new FlamingWeaponDecorator(new FineWeaponDecorator(new Spear))

    weapon.name should be ("Flaming Fine Spear")
    weapon.id should be ("SPEAR")
    weapon.damage should be ((2,4))
    weapon.attackBonus should be (+2)
  }

  it should "pick up a weapon" in {
    val weapon: Weapon = new FineWeaponDecorator(new ShortSword)
    val player: Player = new Player("Test Player", "Klass", "Race")

    player.weapon = new Dagger

    weapon.interact(player)

    player.weapon shouldEqual(weapon)
  }

  it should "use the base weapons droppable status" in {
    val weapon = new FineWeaponDecorator(new ShortSword)
    weapon.isDroppable shouldBe true
  }

  it should "set weapon to enchanted when decorated" in {
    var weapon = new Dagger

  }
}
