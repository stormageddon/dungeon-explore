package com.scalaplayground.dungeonexplore


import com.scalaplayground.dungeonexplore.Armor._
import com.scalaplayground.dungeonexplore.Consumables.{FirePotion, HardenedArmorPotion, HealthPotion, IdentifyScroll, PoisonPotion, TelepathyPotion, TeleportScroll}
import com.scalaplayground.dungeonexplore.Item._
import com.scalaplayground.dungeonexplore.Monster.CharacterObject
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.Weapons._
import com.scalaplayground.dungeonexplore.constants.Constants
import com.scalaplayground.dungeonexplore.constants.Constants._
import net.team2xh.scurses.{Colors, Scurses}

import scala.util.Random

class Player(val name:String, val charClass:String, val charRace:String) extends CharacterObject {
  var health = STARTING_PLAYER_HEALTH
  var maxHealth = STARTING_PLAYER_HEALTH
  var level = 1
  var armorClass = 10
  var attackBonus = 2
  var sightDistance = DEFAULT_SIGHT_DISTANCE
  var armor: Armor = new Natural
  var position = new Position(10, 14)
  val displayChar = "@"
  var actionMessages = Seq[String]()
  var canAvoidObstacles = false
  val inventory = new Inventory
  var weapon: Weapon = new Dagger

  initializeInventory

  def render(screen:Scurses) = {
    screen.put(position.x, position.y, displayChar, Colors.DIM_GREEN)
  }

  def calculateDamage: Int = {
    weapon.attack()
  }

  def performAttack(targetAC: Int): Int = {
    val attackRoll = Random.nextInt(20) + weapon.attackBonus + attackBonus + 1
    attackRoll
  }

  def donArmor(newArmor: Armor) = {
    armor = newArmor
  }

  def consumeConsumable(consumable:Any): Boolean = {
    consumable match {
      case potion:HealthPotion => {
        appendActionMessage(potion.consume(this))
        inventory.remove(potion.id)
        HealthPotion.isIdentified = true
        return true
      }
      case potion:HardenedArmorPotion => {
        appendActionMessage(potion.consume(this))
        inventory.remove(potion.id)
        HardenedArmorPotion.isIdentified = true
      }
      case potion:PoisonPotion => {
        appendActionMessage(potion.consume(this))
        inventory.remove(potion.id)
        PoisonPotion.isIdentified = true
      }
      case potion:TelepathyPotion => {
        appendActionMessage(potion.consume(this))
        inventory.remove(potion.id)
        TelepathyPotion.isIdentified = true
      }
      case potion:FirePotion => {
        appendActionMessage(potion.consume(this))
        inventory.remove(potion.id)
        FirePotion.isIdentified = true
      }
      case scroll:IdentifyScroll => {
        appendActionMessage(scroll.consume(this))
        inventory.remove(scroll.id)
        IdentifyScroll.isIdentified = true
      }
      case scroll:TeleportScroll => {
        appendActionMessage(scroll.consume(this))
        inventory.remove(scroll.id)
        TeleportScroll.isIdentified = true
      }

    }

    return false
  }

  def move(xVel: Int, yVel: Int): Position = {
    new Position(DungeonHelper.clamp(this.position.x + xVel, 0, NUM_COLS - 1), DungeonHelper.clamp(this.position.y + yVel, 0, NUM_ROWS - 1))
  }

  def endRound = {
    actionMessages = Seq[String]()
  }

  def appendActionMessage(message:String): Unit = {
    actionMessages = actionMessages :+ message
  }

  private def initializeInventory: Unit = {
    var weapon: Weapon = this.weapon
    weapon.position = new Position(-1, -1)

    charClass match {
      case Classes.ALCHEMIST => {
        HealthPotion.isIdentified = true
        inventory.add(new HealthPotion(new Position(-1, -1)))
      }
      case Classes.WIZARD => {
        IdentifyScroll.isIdentified = true
        inventory.add(new IdentifyScroll(new Position(-2, -2)))
      }
      case _ =>
    }
    inventory.add(weapon)
  }
}

