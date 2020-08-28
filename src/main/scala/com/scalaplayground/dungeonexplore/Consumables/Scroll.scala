package com.scalaplayground.dungeonexplore.Consumables

import com.scalaplayground.dungeonexplore.Monster.CharacterObject
import com.scalaplayground.dungeonexplore.{FirePotion, HardenedArmorPotion, HealthPotion, Player, PoisonPotion, TelepathyPotion}
import com.scalaplayground.dungeonexplore.Position.Position

import scala.util.Random

sealed trait Scroll extends Consumable {
  override val displayChar: String = "!"
  def description: String = if (identified) name else s"cracked scroll"
  def consume(target: CharacterObject): String

  override def interact(target: Player): Unit = {
    target.appendActionMessage(s"Picked up ${this.description}")
    target.inventory.add(this)
  }
}

object Scroll {
  def initializeScrolls: Unit = {
    def randStr(len:Int): String = s"${Random.alphanumeric.filter(_.isLetter).take(len).mkString.toUpperCase}"

    IdentifyScroll.hiddenName = s"Scroll of ${randStr(4)}"
  }

  def generateScroll(pos:Position): Scroll = {
    Random.nextInt(100) match {
      case roll if 0 until 100 contains roll => new IdentifyScroll(pos)
    }
  }
}

class IdentifyScroll(pos:Position) extends Scroll {
  id = "SCROLL_IDENTIFY"
  name = "Scroll of Identify"
  override def description: String = if (IdentifyScroll.isIdentified) name else IdentifyScroll.hiddenName

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
