package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Item.Item
import com.scalaplayground.dungeonexplore.Position.Position
import org.scalatest.{FlatSpec, Matchers}

class InventoryTest extends FlatSpec with Matchers {
  behavior of "Inventory test"

  it should "Add an item to the inventory" in {
    // arrange
    val inventory = new Inventory

    // act
    inventory.add(new Item(new Position(1, 1)))

    // assert
    inventory.getItems.size shouldBe 1
  }

  it should "not add an item to the inventory if the total weight would be over inventory capacity" in {
    val inventory = new Inventory

    inventory.add(new Item(new Position(1, 1), weight = 55.0))
    inventory.add(new Item(new Position(2,2), weight = 10))

    inventory.getItems.size shouldBe 1
  }

  it should "track the number of an item within the inventory" in {
    val inventory = new Inventory

    inventory.add(new Item(new Position(1, 1), id = "POTION"))
    inventory.add(new Item(new Position(1, 1), id = "POTION"))
    inventory.add(new Item(new Position(-1, -1), id = "OTHER_ITEM"))

    inventory.items("POTION") shouldBe 2
    inventory.items("OTHER_ITEM") shouldBe 1
  }
}
