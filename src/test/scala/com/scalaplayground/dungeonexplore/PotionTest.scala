package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Consumables.HealthPotion
import com.scalaplayground.dungeonexplore.Position.Position
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers}

class PotionTest extends FlatSpec
  with Matchers
  with BeforeAndAfterEach
  with BeforeAndAfterAll {

  behavior of "Potion test"

  var testPlayer:Player = new Player("testName", "testClass", "testRace")

  override def beforeAll: Unit = {
    HealthPotion.isIdentified = false
  }

  override def beforeEach: Unit = {
    testPlayer.inventory.clear
  }

  it should "Create a potion" in {
    // arrange
    val potion = new HealthPotion(new Position(1,1))

    // act
    potion.interact(testPlayer)

    // assert
    potion.name shouldBe "Healing Potion"
    potion.id shouldBe "POTION_HEALTH"
    potion.color shouldBe "red"
    potion.description shouldBe "swirling red potion"
    testPlayer.actionMessages.head shouldBe "Picked up swirling red potion"
  }

  it should "Show the hidden name when potion is unidentified" in {
    val potion = new HealthPotion(new Position(1,1))

    HealthPotion.isIdentified shouldBe false
    potion.description shouldBe "swirling red potion"

    HealthPotion.isIdentified = true

    potion.description shouldBe "Healing Potion"
  }
}
