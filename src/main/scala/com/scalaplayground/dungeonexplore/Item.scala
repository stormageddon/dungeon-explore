package com.scalaplayground.dungeonexplore.Item

import com.scalaplayground.dungeonexplore.Armor._
import com.scalaplayground.dungeonexplore.{DungeonHelper, Player}
import com.scalaplayground.dungeonexplore.Position.Position
import net.team2xh.scurses.{Scurses}

class Item(startingPos: Position = new Position(-1, -1),
           dispChar: String = "!",
           hoverDescription: String = "A swirling potion lies here.",
           itemId: String = "POTION",
           isIdentified: Boolean = true
          ) {

  var position: Position = startingPos
  val displayChar: String = dispChar
  val tileDescription: String = hoverDescription
  var id: String = itemId
  val dungeonHelper = new DungeonHelper
  val identified: Boolean = isIdentified

  def render(x: Int, y: Int, screen: Scurses): Unit = {
    screen.put(x, y, displayChar)
  }

  def interact(target: Player): Unit = {
    id match {
      case "POTION" => {
        target.numPotions = target.numPotions + 1
        target.appendActionMessage("You picked up a potion!")
      }
      case "RING_OF_HEALTH" => {
        target.maxHealth = target.maxHealth + 10
        target.health = target.health + 10
      }
      case "LEATHER_ARMOR" => {
        target.appendActionMessage("You don the Leather Armor. You look sexy.")
        target.donArmor(new Leather)
      }
      case "CHAINMAIL" => {
        target.appendActionMessage("You don the Chain Mail. It's a bit snug.")
        target.donArmor(new Chain)
      }
      case "PLATE_MAIL" => {
        target.appendActionMessage("You don the Plate Mail. It feels heavy, yet protective.")
        target.donArmor(new PlateMail)
      }
      case "DRAGON_SCALE" => {
        target.appendActionMessage("You don the Dragon Scale. You feel totally protected.")
        target.donArmor(new DragonScale)
      }
    }
  }
}
