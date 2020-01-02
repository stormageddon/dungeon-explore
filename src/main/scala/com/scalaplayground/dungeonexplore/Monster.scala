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
  var position: Position = new Position(Random.nextInt(NUM_ROWS), Random.nextInt(NUM_COLS))
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

  def dropLoot: Option[(String, String)] = {
    val roll = Random.nextInt(100)
    if (roll <= POTION_DROP_PERCENTAGE) {
      return Some("POTION", "A swirling potion lies here.")
    }
    else if (roll <= WEAPON_DROP_PERCENTAGE && weapon.isDroppable) {
      return Some((weapon.id, weapon.name))
    }
    else if (roll <= ARMOR_DROP_PERCENTAGE && armor.isDroppable) {
      return Some((armor.id, armor.name))
    }
    None
  }

  def move(target: Option[Position]): Position = {
    val newPosition = this.position
    target match {
      case Some(p) => {
        // try horizontal first
        if (this.position.x < p.x - 1) {
          this.position.x += 1
        }
        else if (this.position.x > p.x + 1) {
          this.position.x -= 1
        }
        // then try vertical
        else if (this.position.y < p.y - 1) {
          this.position.y += 1
        }
        else if (this.position.y > p.y + 1) {
          this.position.y -= 1
        }
      }
      case None => Unit
    }
    //    val x = dungeonHelper.clamp(this.position.x + 1, 0, NUM_ROWS)
    //    val y = dungeonHelper.clamp(this.position.y + 1, 0, NUM_ROWS)
    newPosition
  }
}

class GiantRat extends Monster {
  override val name = "Giant Rat"
  override var health = 1
  weapon = new Claws
  weapon.damage = (1,2)
  position = new Position(1,1)
  displayChar = "r"

  override def move(target: Option[Position]): Position = {
    val possibleMoves = Seq(
      new Position(this.position.x + 1, this.position.y),
      new Position(this.position.x - 1, this.position.y),
      new Position(this.position.x, this.position.y + 1),
      new Position(this.position.x, this.position.y - 1),
      new Position(this.position.x + 1, this.position.y + 1),
      new Position(this.position.x - 1, this.position.y - 1)
    )

    val p = possibleMoves(Random.nextInt(possibleMoves.length))
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
  displayChar = "g"
}

class Wolf extends Monster {
  override val name = "Wolf"
  override var health = 3
  armorClass = 10
  weapon = new Claws
  displayChar = "w"
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
  displayChar = "k"
}

class Orc(startingPos: Position = new Position(0, 0)) extends Monster {
  override val name = "Orc"
  override var health = 3
  position = startingPos
  weapon = List(
    new RustyShortSword,
    new ShortSword,
    new FineShortSword,
    new RustyGreatAxe,
    new GreatAxe,
    new FineGreatAxe
  )(Random.nextInt(6))
  armor = List(new Natural, new Leather, new Chain, new PlateMail)(Random.nextInt(4))
  displayChar = "o"

  override def move(target: Option[Position]): Position = {
    val newPosition = this.position
    target match {
      case Some(p) => {
        // try horizontal first
        if (this.position.x < p.x) {
          this.position.x += 1
        }
        else if (this.position.x > p.x) {
          this.position.x -= 1
        }
        // then try vertical
        else if (this.position.y < p.y) {
          this.position.y += 1
        }
        else if (this.position.y > p.y) {
          this.position.y -= 1
        }
      }
      case None => Unit
    }
//    val x = dungeonHelper.clamp(this.position.x + 1, 0, NUM_ROWS)
//    val y = dungeonHelper.clamp(this.position.y + 1, 0, NUM_ROWS)
    newPosition
  }
}

class CemHial extends Monster {
  override val name = "Cem Hial, the Necromancer"
  override var health = 10
  weapon = new NightBlade
  armorClass = 13
  displayChar = "C"
}

class DireWolf extends Monster {
  override val name = "Dire Wolf"
  override var health = 7
  weapon = new Claws()
  weapon.damage = (1,6)
  displayChar = "W"

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
  displayChar = "G"
}



