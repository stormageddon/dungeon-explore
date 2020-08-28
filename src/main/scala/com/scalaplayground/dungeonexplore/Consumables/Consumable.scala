package com.scalaplayground.dungeonexplore.Consumables

import com.scalaplayground.dungeonexplore.Item.Item
import com.scalaplayground.dungeonexplore.Monster.CharacterObject
import net.team2xh.scurses.Scurses

trait Consumable extends Item{
  def description:String
  def consume(target: CharacterObject): String
}
