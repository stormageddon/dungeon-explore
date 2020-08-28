package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Item.Item
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.Weapons.Dagger
import org.scalatest.{FlatSpec, Matchers}

class InventoryTest extends FlatSpec with Matchers {
  behavior of "Inventory test"

  it should "Add an item to the inventory" in {
    // arrange
    val inventory = Inventory()

    // act
    inventory.add(new Item(new Position(1, 1)))

    // assert
    inventory.getItems.size shouldBe 1
  }

  it should "not add an item to the inventory if the total weight would be over inventory capacity" in {
    val inventory = Inventory()

    inventory.add(new Item(new Position(1, 1), id = "ITEM1", weight = 55.0))
    inventory.add(new Item(new Position(2,2), id = "ITEM2", weight = 10))

    inventory.getItems.size shouldBe 1
    inventory.totalCapacity shouldBe 55
  }

  it should "track the number of an item within the inventory" in {
    val inventory = Inventory()

    inventory.add(new Item(new Position(1, 1), id = "POTION"))
    inventory.add(new Item(new Position(1, 1), id = "POTION"))
    inventory.add(new Item(new Position(-1, -1), id = "OTHER_ITEM"))

    inventory.items("POTION").size shouldBe 2
    inventory.items("OTHER_ITEM").size shouldBe 1
  }

  it should "remove an item from inventory" in {
    val inventory = Inventory()
    inventory.add(new Item(new Position(1, 1), id = "POTION"))
    inventory.add(new Item(new Position(1, 1), id = "POTION"))

    inventory.remove("POTION")
    inventory.items("POTION").size shouldBe 1
  }

  it should "remove the item entirely from inventory if the last of that item is removed" in {
    val inventory = Inventory()

    inventory.add(new Item(new Position(1, 1), id = "POTION"))
    inventory.remove("POTION")

    inventory.items.contains("POTION") shouldBe false
  }

  it should "Decrease total capacity by removed items weight" in {
    val inventory = Inventory()
    inventory.maxCapacity = 100

    val item1 = new Item(new Position(1, 1), id = "POTION", weight = 25)
    val item2 = new Item(new Position(1, 1), id = "OTHER ITEM", weight = 50)

    inventory.add(item1)
    inventory.add(item2)

    inventory.remove(item1.id)

    inventory.totalCapacity shouldBe 50
  }

  it should "Get item from inventory by index" in {
    val inventory = Inventory()

    val item1 = new Item(new Position(1, 1), id = "POTION")
    val item2 = new Item(new Position(1, 1), id = "POTION")
    val item3 = new Item(new Position(1, 1), id = "POTION")
    val item4 = new Item(new Position(1, 1), id = "OTHER ITEM")

    inventory.add(item1)
    inventory.add(item2)
    inventory.add(item3)
    inventory.add(item4)

    var result = inventory.getItem(0)
    result.id shouldBe item1.id

    result = inventory.getItem(1)
    result.id shouldBe item4.id
  }

  it should "Get item from inventory by id" in {
    val inventory = Inventory()

    val item1 = new HealthPotion(new Position(1,1))
    val item2 = new Dagger
    val item3 = new TelepathyPotion(new Position(1,1))

    inventory.add(item1)
    inventory.add(item2)
    inventory.add(item3)

    var result = inventory.getItem("POTION_HEALTH")
    result shouldBe item1

    result = inventory.getItem("POTION_TELEPATHY")
    result shouldBe item3

    result = inventory.getItem("DAGGER")
    result shouldBe item2
  }
}

