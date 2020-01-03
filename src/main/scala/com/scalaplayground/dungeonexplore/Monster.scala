package com.scalaplayground.dungeonexplore.Monster

import com.scalaplayground.dungeonexplore.Armor._

import scala.util.Random
import com.scalaplayground.dungeonexplore.constants.Constants._
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
    //else if (roll <= WEAPON_DROP_PERCENTAGE && weapon.isDroppable) {
    //  return Some((weapon.id, weapon.name))
    //}
    if (roll <= weapon.dropChance && weapon.isDroppable) {
      return Some((weapon.id, weapon.name))
    }
    else if (roll <= armor.dropChance && armor.isDroppable) {
      return Some((armor.id, armor.name))
    }
    else if (roll <= POTION_DROP_PERCENTAGE) {
      return Some("POTION", "A swirling potion lies here.")
    }
    None
  }

  def move(target: Option[Position]): Position = {

    val newPosition: Position = target match {
      case Some(p) => {
        // try horizontal first
        var calculatedPos = new Position(-1, -1)
        if (position.x < p.x - 1) {
          calculatedPos = new Position(position.x + 1, position.y)
        }
        else if (position.x > p.x + 1) {
          calculatedPos = new Position(position.x - 1, position.y)
        }
        // then try vertical
        else if (position.y < p.y - 1) {
          calculatedPos = new Position(position.x, position.y + 1)
        }
        else if (position.y > p.y + 1) {
          calculatedPos = new Position(position.x, position.y - 1)
        }

        calculatedPos
      }
      case None => position
    }

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
}

class CemHial extends Monster {
  override val name = "Cem Hial, the Necromancer"
  override var health = 20
  weapon = new NightBlade
  armorClass = 15
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
    var damage:Int = super.calculateDamage
    if (roll <= 1) {
      damage = damage + super.calculateDamage
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

class Dragon extends Monster {
  override val name = "Young Black Dragon"
  override var health = 15
  weapon = new Claws()
  weapon.damage = (3,8)
  displayChar = "D"
  armorClass = 13
  armor = new DragonScale()

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
    // move twice
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

    newPosition
  }

  override def performAttack: Int = {

    class FireBreath extends Weapon {
      var name = "Fire breath"
      var damage = (5,10)
      var id = "FIRE_BREATH"
      isDroppable = false
      attackBonus = 2
      attackText = "opens its maw and releases a column of burning flames"
    }

    val dragonClaws = new Claws()
    dragonClaws.damage = (3,8)

    val fireBreath = new FireBreath()



    val possibleAttacks = Seq(
      dragonClaws,
      fireBreath
    )
    weapon = possibleAttacks(Random.nextInt(possibleAttacks.length))

    val attackRoll = Random.nextInt(20) + weapon.attackBonus + attackBonus + 1
    attackRoll
  }

  override def calculateDamage: Int = {
    val damage = Random.nextInt(weapon.damage._2) + weapon.damage._1
    damage
  }
}



