package com.scalaplayground.dungeonexplore


import com.scalaplayground.dungeonexplore.Armor._
import com.scalaplayground.dungeonexplore.Weapon._
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.constants.Constants._

import scala.util.Random

class Player(val name:String) {
  var health = STARTING_PLAYER_HEALTH
  var maxHealth = STARTING_PLAYER_HEALTH
  var weapon: Weapon = new RustyDagger
  var numPotions = 1
  val armorClass = 10
  var attackBonus = 2
  var armor: Armor = new Natural
  var position = new Position(10, 10)
  var dungeonHelper = new DungeonHelper
  val displayChar = "@"


  def render = {
    print(displayChar)
  }

  def calculateDamage: Int = {
    weapon.attack
  }

  def performAttack: Int = {
    println("You attack!")
    val attackRoll = Random.nextInt(20) + weapon.attackBonus + attackBonus + 1
    attackRoll
  }

  def donArmor(newArmor: Armor) = {
    armor = newArmor
  }

  def quaffPotion: Unit = {
    if (numPotions > 0) {
      val healthRegained = dungeonHelper.clamp(Random.nextInt(6) + 1, 0 , maxHealth)
      numPotions = numPotions - 1
      println(s"You quickly quaff a potion, regaining ${healthRegained} health. You have ${numPotions} left")
      health = health + healthRegained
    }
    else {
      println("You are out of potions!")
    }
  }

  def move(xVel: Int, yVel: Int): Position = {
    new Position(dungeonHelper.clamp(this.position.x + xVel, 0, NUM_ROWS), dungeonHelper.clamp(this.position.y + yVel, 0, NUM_COLS))
  }
}

