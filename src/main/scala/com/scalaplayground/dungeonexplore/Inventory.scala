package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Item.Item

case class Inventory() {
  var items = Map[String, Seq[Item]]()
  var totalCapacity = 0.0
  var maxCapacity = 60.0


  def add(item: Item) = {
    if (item.weight + totalCapacity <= maxCapacity) {
      if (items.contains(item.id)) {
        items = items + (item.id -> (items.get(item.id).getOrElse(Seq[Item]()) :+ item))
        totalCapacity = totalCapacity + item.weight
      }
      else {
        items = items + (item.id -> Seq[Item](item))
        totalCapacity = totalCapacity + item.weight
      }
    }
  }

  def remove(itemId: String) = {
    if (items.contains(itemId)) {
      val removedItem = items.get(itemId).get.head
      items = items + (itemId -> (items.get(itemId).getOrElse(Seq[Item]()).tail))
      if (items.get(itemId).getOrElse(Seq()).isEmpty) {
        items = items.-(itemId)
      }
      totalCapacity = totalCapacity - removedItem.weight
    }
  }

  def getItems: Map[String, Seq[Item]] = {
    return items
  }
}
