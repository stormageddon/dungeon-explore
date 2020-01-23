package com.scalaplayground.dungeonexplore.Item

import com.scalaplayground.dungeonexplore.Armor._
import com.scalaplayground.dungeonexplore.{DungeonHelper, Player}
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.Weapon._
import net.team2xh.scurses.{Colors, Scurses}

class Item(startingPos: Position, dispChar: String, hoverDescription: String = "A swirling potion lies here.", itemId: String = "POTION", isIdentified: Boolean = true) {
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
      case "RUSTY_DAGGER" => {
        target.appendActionMessage("You picked up the Rusty Dagger")
        target.weapon = new RustyDagger
      }
      case "DAGGER" => {
        target.appendActionMessage("You picked up the Dagger")
        target.weapon = new Dagger
      }
      case "FINE_DAGGER" => {
        target.weapon = new FineDagger
        target.appendActionMessage("You picked up the Fine Dagger")
      }
      case "RUSTY_SHORT_SWORD" => {
        target.appendActionMessage("You picked up the Rusty Short Sword")
        target.weapon = new RustyShortSword
      }
      case "SHORT_SWORD" => {
        target.appendActionMessage("You picked up the Short Sword")
        target.weapon = new ShortSword
      }
      case "FINE_SHORT_SWORD" => {
        target.appendActionMessage("You picked up the Fine Short Sword")
        target.weapon = new FineShortSword
      }
      case "RUSTY_GREAT_AXE" => {
        target.appendActionMessage("You picked up the Rusty Great Axe")
        target.weapon = new RustyGreatAxe
      }
      case "GREAT_AXE" => {
        target.appendActionMessage("You picked up the Great Axe")
        target.weapon = new GreatAxe
      }
      case "FINE_GREAT_AXE" => {
        target.appendActionMessage("You picked up the Fine Great Axe")
        target.weapon = new FineGreatAxe
      }
      case "SPEAR" => {
        target.appendActionMessage("You picked up the Spear")
        target.weapon = new Spear
      }
      case "NIGHT_BLADE" => {
        target.appendActionMessage("You hold the gleaming blade aloft. Outside, the clouds part for the first time in centuries. The town rejoices.\nThe heroes have won.... for now.")
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
