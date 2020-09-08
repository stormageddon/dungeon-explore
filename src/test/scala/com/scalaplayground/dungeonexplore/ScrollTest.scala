package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Consumables.{FirePotion, HardenedArmorPotion, HealthPotion, IdentifyScroll, PoisonPotion, Potion, Scroll, TelepathyPotion}
import com.scalaplayground.dungeonexplore.Game.GameState
import com.scalaplayground.dungeonexplore.Position.Position
import net.team2xh.scurses.Scurses
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

class ScrollTest extends AnyFlatSpec
  with Matchers
  with BeforeAndAfterEach
  with BeforeAndAfterAll {

  behavior of "Scroll Test"

  var testPlayer:Player = new Player("testName", "testClass", "testRace")

  override def beforeAll: Unit = {
    val s: Scurses = new Scurses
    Scroll.initializeScrolls(new GameState(testPlayer, s))
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
    testPlayer.inventory.getItem("POTION_HEALTH").asInstanceOf[Potion].description mustBe "Healing Potion"
    testPlayer.inventory.getItem("POTION_STONE_SKIN").asInstanceOf[Potion].description mustBe "Stone Skin Potion"
    testPlayer.inventory.getItem("POTION_POISON").asInstanceOf[Potion].description mustBe "Poison"
    testPlayer.inventory.getItem("POTION_TELEPATHY").asInstanceOf[Potion].description mustBe "Telepathy Potion"
    testPlayer.inventory.getItem("POTION_FIRE").asInstanceOf[Potion].description mustBe "Fire Potion"
  }

  it should "reveal the name when identified" in {
    testPlayer.inventory.add(new IdentifyScroll(new Position(1,1)))

    var testScroll:IdentifyScroll = testPlayer.inventory.getItem("SCROLL_IDENTIFY").asInstanceOf[IdentifyScroll]
    testScroll.description mustNot be ("Scroll of Identify")

    IdentifyScroll.isIdentified = true

    testScroll.description mustBe "Scroll of Identify"
  }
}
