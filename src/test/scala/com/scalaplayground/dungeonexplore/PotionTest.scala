package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Consumables.HealthPotion
import com.scalaplayground.dungeonexplore.Position.Position
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

class PotionTest extends AnyFlatSpec
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
    HealthPotion.assignedColor = "red"
    val potion = new HealthPotion(new Position(1,1))

    // act
    potion.interact(testPlayer)

    // assert
    potion.name mustBe "Healing Potion"
    potion.id mustBe "POTION_HEALTH"
    potion.color mustBe "red"
    potion.description mustBe "swirling red potion"
    testPlayer.actionMessages.head mustBe "Picked up swirling red potion"
  }

  it should "Show the hidden name when potion is unidentified" in {
    HealthPotion.assignedColor = "red"
    val potion = new HealthPotion(new Position(1,1))

    HealthPotion.isIdentified mustBe false
    potion.description mustBe "swirling red potion"

    HealthPotion.isIdentified = true

    potion.description mustBe "Healing Potion"
  }
}
