package com.scalaplayground.dungeonexplore.Monster

import com.scalaplayground.dungeonexplore.Armor._

import scala.util.Random
import com.scalaplayground.dungeonexplore.Constants._
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.Weapon._
import com.scalaplayground.dungeonexplore.DungeonHelper

abstract class Monster {
  val name: String
  var health: Int
  var armorClass: Int = 8
  var attackBonus: Int = 1
  var weapon: Weapon = new RustyDagger
  var armor: Armor = new Natural
  var position: Position = new Position(10,10)
  var displayChar: String = "m"
  var dungeonHelper = new DungeonHelper

  def isAlive: Boolean = {
    health > 0
  }

  def performAttack: Int = {
    val attackRoll = Random.nextInt(20) + weapon.attackBonus + attackBonus + 1
    attackRoll
  }

  def calculateDamage: Int = {
    val damage = Random.nextInt(weapon.damage._2) + weapon.damage._1
    damage
  }

  def dropLoot: Option[String] = {
    val roll = Random.nextInt(100)
    if (roll <= POTION_DROP_PERCENTAGE) {
      return Some("potion")
    }
    None
  }

  def move: Position = {
    this.position
  }
}

class GiantRat extends Monster {
  override val name = "Giant Rat"
  override var health = 1
  weapon = new Claws
  weapon.damage = (1,2)
  position = new Position(1,1)
  displayChar = "r"

  override def move: Position = {
    val possibleMoves = Seq(
      new Position(this.position.x + 1, this.position.y),
      new Position(this.position.x - 1, this.position.y),
      new Position(this.position.x, this.position.y + 1),
      new Position(this.position.x, this.position.y - 1),
      new Position(this.position.x + 1, this.position.y + 1),
      new Position(this.position.x - 1, this.position.y - 1)
    )

    var p = possibleMoves(Random.nextInt(possibleMoves.length))
    p.x = dungeonHelper.clamp(p.x, 0, NUM_ROWS)
    p.y = dungeonHelper.clamp(p.y, 0, NUM_COLS)
    p
  }
}

class Goblin extends Monster {
  override val name = "Goblin"
  override var health = 1
  attackBonus = 2
  weapon = List(new RustyDagger,new Dagger,new FineDagger)(Random.nextInt(3))
  armor = List(new Natural, new Leather)(Random.nextInt(2))
}

class Wolf extends Monster {
  override val name = "Wolf"
  override var health = 3
  armorClass = 10
  weapon = new Claws
}

class Kobold extends Monster {
  override val name = "Kobold"
  override var health = 2
  weapon = List(
    new Spear,
    new RustyShortSword,
    new ShortSword,
    new RustyDagger,
    new Dagger
  )(Random.nextInt(5))
  armor = List(new Natural, new Leather)(Random.nextInt(2))
}

class Orc extends Monster {
  override val name = "Orc"
  override var health = 3
  weapon = List(
    new RustyShortSword,
    new ShortSword,
    new FineShortSword,
    new RustyGreatAxe,
    new GreatAxe,
    new FineGreatAxe
  )(Random.nextInt(6))
  armor = List(new Natural, new Leather, new Chain, new PlateMail)(Random.nextInt(4))
}

class CemHial extends Monster {
  override val name = "Cem Hial, the Necromancer"
  override var health = 10
  weapon = new NightBlade
  armorClass = 13
}

class DireWolf extends Monster {
  override val name = "Dire Wolf"
  override var health = 7
  weapon = new Claws()
  weapon.damage = (1,6)

  override def calculateDamage: Int = {
    val roll = Random.nextInt(10)
    var damage:Int = super.performAttack
    if (roll <= 1) {
      println("The Dire Wolf snaps ferociously!")
      damage = damage + super.performAttack
    }
    damage
  }
}

class RockGolem extends Monster {
  override val name = "Rock Golem"
  override var health = 8
  weapon = new Claws()
  weapon.damage = (1,8)
}



