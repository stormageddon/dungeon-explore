package com.scalaplayground.dungeonexplore


import com.scalaplayground.dungeonexplore.Armor._
import com.scalaplayground.dungeonexplore.Item._
import com.scalaplayground.dungeonexplore.Monster.CharacterObject
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.Weapons._
import com.scalaplayground.dungeonexplore.constants.Constants._
import net.team2xh.scurses.{Colors, Scurses}

import scala.util.Random

class Player(val name:String, val charClass:String, val charRace:String) extends CharacterObject {
  var health = STARTING_PLAYER_HEALTH
  var maxHealth = STARTING_PLAYER_HEALTH
  var level = 1
  var weapon: Weapon = new RustyWeaponDecorator(new Dagger)
  val armorClass = 10
  var attackBonus = 2
  var sightDistance = 4
  var armor: Armor = new Natural
  var position = new Position(10, 14)
  val displayChar = "@"
  var actionMessages = Seq[String]()
  var canAvoidObstacles = false
  val inventory = new Inventory
  inventory.add(new Item(new Position(-1,-1), id = "POTION", name = "Health Potion"))
  weapon.position = new Position(-1, -1)
  inventory.add(weapon)


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

  def quaffPotion: Boolean = {
    if (inventory.items.get("POTION").getOrElse(Seq()).nonEmpty) {
      val healthRegained = Random.nextInt(6) + 1
      inventory.remove("POTION")
      appendActionMessage(s"You quickly quaff a potion, regaining ${healthRegained} health. You have ${inventory.items.get("POTION").getOrElse(Seq()).size} left")
      health = DungeonHelper.clamp(health + healthRegained, 0, maxHealth)
    }
    else {
      appendActionMessage("You are out of potions!")
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
}

