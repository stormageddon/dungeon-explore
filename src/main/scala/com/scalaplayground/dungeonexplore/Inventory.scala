package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Item._

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

  def clear: Unit = {
    items.empty
  }

  def getItems: Map[String, Seq[Item]] = {
    return items
  }

  def getItem(index: Int): Item = {
    items.toList(index)._2.head
  }

  def getItem(id:String): Item = {
    items.filter(_._1 == id).head._2.head
  }
}
