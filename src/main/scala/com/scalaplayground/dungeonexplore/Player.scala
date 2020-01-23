package com.scalaplayground.dungeonexplore


import com.scalaplayground.dungeonexplore.Armor._
import com.scalaplayground.dungeonexplore.Weapon._
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.constants.Constants._
import net.team2xh.scurses.{Colors, Scurses}

import scala.util.Random

class Player(val name:String, val charClass:String, val charRace:String) {
  var health = STARTING_PLAYER_HEALTH
  var maxHealth = STARTING_PLAYER_HEALTH
  var level = 1
  var weapon: Weapon = new RustyDagger
  var numPotions = 1
  val armorClass = 10
  var attackBonus = 2
  var sightDistance = 4
  var armor: Armor = new Natural
  var position = new Position(10, 14)
  var dungeonHelper = new DungeonHelper
  val displayChar = "@"
  var actionMessage: String = ""
  var canAvoidObstacles = false


  def render(screen:Scurses) = {
    screen.put(position.x, position.y, displayChar, Colors.DIM_GREEN)//dungeonHelper.padGameObjectChar(displayChar))
  }

  def calculateDamage: Int = {
    weapon.attack
  }

  def performAttack(targetAC: Int): Int = {
    val attackRoll = Random.nextInt(20) + weapon.attackBonus + attackBonus + 1
    appendActionMessage(s"You swing with a roll of ${attackRoll} vs the monster's ac of ${targetAC}")
    attackRoll
  }

  def donArmor(newArmor: Armor) = {
    armor = newArmor
  }

  def quaffPotion: Boolean = {
    if (numPotions > 0) {
      val healthRegained = Random.nextInt(6) + 1
      numPotions = numPotions - 1
      appendActionMessage(s"You quickly quaff a potion, regaining ${healthRegained} health. You have ${numPotions} left")
      health = dungeonHelper.clamp(health + healthRegained, 0, maxHealth)
    }
    else {
      appendActionMessage("You are out of potions!")
    }
    return false
  }

  def move(xVel: Int, yVel: Int): Position = {
    new Position(dungeonHelper.clamp(this.position.x + xVel, 0, NUM_COLS - 1), dungeonHelper.clamp(this.position.y + yVel, 0, NUM_ROWS - 1))
  }

  def endRound = {
    actionMessage = ""
  }

  def appendActionMessage(message:String): Unit = {
    actionMessage = actionMessage + message
  }
}

