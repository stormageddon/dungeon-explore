package com.scalaplayground.dungeonexplore.Item

import com.scalaplayground.dungeonexplore.Armor._
import com.scalaplayground.dungeonexplore.{DungeonHelper, Player}
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.Weapons._
import net.team2xh.scurses.{Colors, Scurses}

trait Interactable {
  def interact(target: Player)
}

class Item(startingPos: Position,
           dispChar: String,
           hoverDescription: String = "A swirling potion lies here.",
           itemId: String = "POTION",
           isIdentified: Boolean = true
          ) extends Interactable {

  val position: Position = startingPos
  val displayChar: String = dispChar
  val tileDescription: String = hoverDescription
  val id: String = itemId
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
