package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Consumables.{IdentifyScroll, Scroll}
import com.scalaplayground.dungeonexplore.Position.Position
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, FlatSpec, Matchers}

class ScrollTest extends FlatSpec
  with Matchers
  with BeforeAndAfterEach
  with BeforeAndAfterAll {

  behavior of "Scroll Test"

  var testPlayer:Player = new Player("testName", "testClass", "testRace")

  override def beforeAll: Unit = {
    Scroll.initializeScrolls
  }

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

  it should "reveal the name when identified" in {
    testPlayer.inventory.add(new IdentifyScroll(new Position(1,1)))

    var testScroll:IdentifyScroll = testPlayer.inventory.getItem("SCROLL_IDENTIFY").asInstanceOf[IdentifyScroll]
    testScroll.description should not be "Scroll of Identify"

    IdentifyScroll.isIdentified = true

    testScroll.description should be ("Scroll of Identify")
  }
}
