package com.scalaplayground.dungeonexplore.Consumables

import com.scalaplayground.dungeonexplore.Item.Item
import com.scalaplayground.dungeonexplore.Monster.CharacterObject

trait Consumable extends Item {
  def description:String
  def consume(target: CharacterObject): String
}
