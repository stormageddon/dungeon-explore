package com.scalaplayground.dungeonexplore.Item

import com.scalaplayground.dungeonexplore.Armor._
import com.scalaplayground.dungeonexplore.Player
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.Weapon._

class Item(startingPos: Position, dispChar: String, hoverDescription: String = "A swirling potion lies here.", itemId: String = "POTION") {
  val position: Position = startingPos
  val displayChar: String = dispChar
  val tileDescription: String = hoverDescription
  val id: String = itemId

  def render(): Unit = {
    print(displayChar)
  }

  def interact(player: Player): Unit = {
    id match {
      case "POTION" => {
        player.numPotions = player.numPotions + 1
        player.appendActionMessage("You picked up a potion!")
      }
      case "RUSTY_DAGGER" => {
        player.appendActionMessage("You picked up the Rusty Dagger")
        player.weapon = new RustyDagger
      }
      case "DAGGER" => {
        player.appendActionMessage("You picked up the Dagger")
        player.weapon = new Dagger
      }
      case "FINE_DAGGER" => {
        player.weapon = new FineDagger
        player.appendActionMessage("You picked up the Fine Dagger")
      }
      case "RUSTY_SHORT_SWORD" => {
        player.appendActionMessage("You picked up the Rusty Short Sword")
        player.weapon = new RustyShortSword
      }
      case "SHORT_SWORD" => {
        player.appendActionMessage("You picked up the Short Sword")
        player.weapon = new ShortSword
      }
      case "FINE_SHORT_SWORD" => {
        player.appendActionMessage("You picked up the Fine Short Sword")
        player.weapon = new FineShortSword
      }
      case "RUSTY_GREAT_AXE" => {
        println("You picked up the Rusty Great Axe")
        player.weapon = new RustyGreatAxe
      }
      case "GREAT_AXE" => {
        player.appendActionMessage("You picked up the Great Axe")
        player.weapon = new GreatAxe
      }
      case "FINE_GREAT_AXE" => {
        player.appendActionMessage("You picked up the Fine Great Axe")
        player.weapon = new FineGreatAxe
      }
      case "SPEAR" => {
        player.appendActionMessage("You picked up the Spear")
        player.weapon = new Spear
      }
      case "NIGHT_BLADE" => {
        player.appendActionMessage("You hold the gleaming blade aloft. Outside, the clouds part for the first time in centuries. The town rejoices.\nThe heroes have won.... for now.")
        player.weapon = new NightBlade
      }
      case "LEATHER_ARMOR" => {
        player.appendActionMessage("You don the Leather Armor. You look sexy.")
        player.donArmor(new Leather)
      }
      case "CHAINMAIL" => {
        player.appendActionMessage("You don the Chain Mail. It's a bit snug.")
        player.donArmor(new Chain)
      }
      case "PLATE_MAIL" => {
        player.appendActionMessage("You don the Plate Mail. It feels heavy, yet protective.")
        player.donArmor(new PlateMail)
      }
      case "DRAGON_SCALE" => {
        player.appendActionMessage("You don the Dragon Scale. You feel totally protected.")
        player.donArmor(new DragonScale)
      }

    }
  }
}
