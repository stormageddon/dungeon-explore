package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Consumables.IdentifyScroll
import com.scalaplayground.dungeonexplore.Position.Position
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class ScrollTest extends FlatSpec
  with Matchers
  with BeforeAndAfterEach {

  behavior of "Scroll Test"

  var testPlayer:Player = new Player("testName", "testClass", "testRace")

  override def beforeEach: Unit = {
    testPlayer.inventory.clear
  }

  behavior of "Identify Scroll"

  it should "identify all potion types when consumed" in {
    // arrange
    testPlayer.inventory.add(new HealthPotion(new Position(1,1)))
    testPlayer.inventory.add(new HardenedArmorPotion(new Position(1,1)))
    testPlayer.inventory.add(new PoisonPotion(new Position(1,1)))
    testPlayer.inventory.add(new TelepathyPotion(new Position(1,1)))
    testPlayer.inventory.add(new FirePotion(new Position(1,1)))

    val testScroll = new IdentifyScroll(new Position(1,1))

    // act
    testScroll.consume(testPlayer)

    // assert
    val inventoryItems = testPlayer.inventory.getItems
    testPlayer.inventory.getItem("POTION_HEALTH").asInstanceOf[Potion].description shouldBe "Healing Potion"
    testPlayer.inventory.getItem("POTION_STONE_SKIN").asInstanceOf[Potion].description shouldBe "Stone Skin Potion"
    testPlayer.inventory.getItem("POTION_POISON").asInstanceOf[Potion].description shouldBe "Poison"
    testPlayer.inventory.getItem("POTION_TELEPATHY").asInstanceOf[Potion].description shouldBe "Telepathy Potion"
    testPlayer.inventory.getItem("POTION_FIRE").asInstanceOf[Potion].description shouldBe "Fire Potion"
  }
}
