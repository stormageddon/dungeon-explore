package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Item.Item

class Inventory {
  var items = Map[String, Int]()
  var totalCapacity = 0.0
  var maxCapacity = 60.0

  def add(item: Item) = {
    if (item.weight + totalCapacity <= maxCapacity) {
      if (items.contains(item.id)) {
        items = items + (item.id -> (items.get(item.id).getOrElse(0) + 1))
        totalCapacity = totalCapacity + item.weight
      }
      else {
        items = items + (item.id -> 1)
      }
    }
  }

  def getItems: Map[String, Int] = {
    return items
  }
}
