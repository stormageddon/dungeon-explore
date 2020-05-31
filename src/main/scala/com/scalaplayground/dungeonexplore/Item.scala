package com.scalaplayground.dungeonexplore.Item

import com.scalaplayground.dungeonexplore.Armor._
import com.scalaplayground.dungeonexplore.{DungeonHelper, Player}
import com.scalaplayground.dungeonexplore.Position.Position
import net.team2xh.scurses.{Colors, Scurses}

class Item(var position: Position = new Position(-1, -1),
           val displayChar: String = "!",
           val tileDescription: String = "A swirling potion lies here.",
           var id: String = "POTION",
           var identified: Boolean = true,
           var enchanted: Boolean = false,
           val weight: Double = 1.0,
           var name: String = "???",
           displayColor: Int = Colors.DIM_WHITE
          ) {

  def render(x: Int, y: Int, screen: Scurses): Unit = {
    screen.put(x, y, displayChar, displayColor)
  }

  def interact(target: Player): Unit = {
    id match {
      case "POTION" => {
        //target.numPotions = target.numPotions + 1
        target.inventory.add(this)
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
