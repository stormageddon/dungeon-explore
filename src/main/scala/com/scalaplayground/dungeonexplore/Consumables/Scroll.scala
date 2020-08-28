package com.scalaplayground.dungeonexplore.Consumables

import com.scalaplayground.dungeonexplore.Game.GameState
import com.scalaplayground.dungeonexplore.Monster.CharacterObject
import com.scalaplayground.dungeonexplore.Player
import com.scalaplayground.dungeonexplore.Position.Position

import scala.util.Random

sealed trait Scroll extends Consumable {
  override val displayChar: String = "!"
  override val tileDescription: String = "a cracked scroll"
  def description: String = if (identified) name else s"cracked scroll"

  override def interact(target: Player): Unit = {
    target.appendActionMessage(s"Picked up ${this.description}")
    target.inventory.add(this)
  }
}

object Scroll {
  def initializeScrolls(gameState:GameState): Unit = {
    def randStr(len:Int): String = s"${Random.alphanumeric.filter(_.isLetter).take(len).mkString.toUpperCase}"

    IdentifyScroll.hiddenName = s"Scroll of ${randStr(4)}"
    TeleportScroll.hiddenName = s"Scroll of ${randStr(4)}"
    TeleportScroll.gameState = gameState
  }

  def generateScroll(pos:Position): Scroll = {
    Random.nextInt(100) match {
      case roll if 0 until 75 contains roll => new IdentifyScroll(pos)
      case roll if 75 until 100 contains roll => new TeleportScroll(pos)
    }
  }
}

class IdentifyScroll(pos:Position) extends Scroll {
  id = "SCROLL_IDENTIFY"
  name = "Scroll of Identify"
  override def description: String = if (IdentifyScroll.isIdentified) name else IdentifyScroll.hiddenName
  position = pos

  def consume(target: CharacterObject): String = {
    target match {
      case p:Player => {
        p.inventory.items.foreach(item => {
          item._2.head.identified = true
          item._2.head match {
            case _:HealthPotion => HealthPotion.isIdentified = true
            case _:HardenedArmorPotion => HardenedArmorPotion.isIdentified = true
            case _:TelepathyPotion => TelepathyPotion.isIdentified = true
            case _:PoisonPotion => PoisonPotion.isIdentified = true
            case _:FirePotion => FirePotion.isIdentified = true
            case _:TeleportScroll => TeleportScroll.isIdentified = true
            case _=>
          }
        })
      }
    }
    "your pack glows with an arcane light"
  }
}

object IdentifyScroll {
  var isIdentified = false
  var hiddenName = ""
}

class TeleportScroll(pos:Position) extends Scroll {
  id = "SCROLL_TELEPORT"
  name = "Scroll of Teleportation"
  position = pos

  override def description: String = if (TeleportScroll.isIdentified) name else TeleportScroll.hiddenName
  def consume(target:CharacterObject): String = {
    val newLocation = TeleportScroll.gameState.getCurrentFloor.getRandomRoom.getRandomValidPosition
    target match {
      case t:Player => {
        t.position = newLocation
        TeleportScroll.gameState.setSurroundingTilesVisible(t.position)
      }
      case _ =>
    }
    "You move to another location with a jolt!"
  }
}

object TeleportScroll {
  var isIdentified = false
  var hiddenName = ""
  var gameState:GameState = null
}
