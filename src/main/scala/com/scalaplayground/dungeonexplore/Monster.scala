package com.scalaplayground.dungeonexplore.Monster

import com.scalaplayground.dungeonexplore.Armor._

import scala.util.Random
import com.scalaplayground.dungeonexplore.constants.Constants._
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.Weapon._
import com.scalaplayground.dungeonexplore.{DungeonHelper, Tile}
import com.scalaplayground.dungeonexplore.PathFinding.{AStar, Dijkstra}

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
  val canAvoidObstacles = false

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

  // DIJKSTRA MOVE

  def move(target: Option[Tile], tiles: Seq[Seq[Tile]], currTile: Option[Tile]): Position = {
    //val dijkstra = new Dijkstra
    val aStar = new AStar

    val nextMove: Option[Tile] = target match {
      case Some(targetTile) => {
        currTile match {
          case Some(currentTile) => {
            //val path = dijkstra.findShortestPath(tiles.flatten, currTile.get, target.get)
            val path = aStar.findShortestPath(tiles.flatten, currTile.get, target.get)
            if (path.size > 1) {
              if (Option(path(1)).get == target.get) {
                Option(path(0))
              }
              else {
                Option(path(1))
              }
            }
            else {
              Option(path(0))
            }
          }
          case None => {
            None
          }
        }
      }
      case None => {
        None
      }
    }

    val newPosition: Position = nextMove match {
      case Some(tile) => {
        tile.position
      }
      case None => position
    }

    newPosition
  }

}

class GiantRat(pos: Position) extends Monster {
  override val name = "Giant Rat"
  override var health = 1
  weapon = new Claws
  weapon.damage = (1,2)
  position = new Position(pos.y, pos.x)
  displayChar = "r"
}

class Goblin(pos: Position) extends Monster {
  override val name = "Goblin"
  override var health = 1
  position = new Position(pos.y, pos.x)
  attackBonus = 2
  weapon = List(new RustyDagger,new Dagger,new FineDagger)(Random.nextInt(3))
  armor = List(new Natural, new Leather)(Random.nextInt(2))
  displayChar = "g"
}

class Wolf(pos: Position) extends Monster {
  override val name = "Wolf"
  override var health = 3
  position = new Position(pos.y, pos.x)
  armorClass = 10
  weapon = new Claws
  displayChar = "w"
}

class Kobold(pos: Position) extends Monster {
  override val name = "Kobold"
  override var health = 2
  position = new Position(pos.y, pos.x)
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

class Orc(pos: Position) extends Monster {
  override val name = "Orc"
  override var health = 3
  position = new Position(pos.y, pos.x)
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

class CemHial(pos: Position) extends Monster {
  override val name = "Cem Hial, the Necromancer"
  override var health = 20
  position = new Position(pos.y, pos.x)
  weapon = new NightBlade
  armorClass = 15
  displayChar = "C"
}

class DireWolf(pos: Position) extends Monster {
  override val name = "Dire Wolf"
  override var health = 7
  weapon = new Claws()
  weapon.damage = (1,6)
  displayChar = "W"
  position = new Position(pos.y, pos.x)

  override def calculateDamage: Int = {
    val roll = Random.nextInt(10)
    var damage:Int = super.calculateDamage
    if (roll <= 1) {
      damage = damage + super.calculateDamage
    }
    damage
  }
}

class RockGolem(pos: Position) extends Monster {
  override val name = "Rock Golem"
  override var health = 8
  position = new Position(pos.y, pos.x)
  weapon = new Claws()
  weapon.damage = (1,8)
  displayChar = "G"
}

class Dragon(pos: Position) extends Monster {
  override val name = "Young Black Dragon"
  override var health = 15
  override val canAvoidObstacles = true
  position = new Position(pos.y, pos.x)
  weapon = new Claws()
  weapon.damage = (3,8)
  displayChar = "D"
  armorClass = 13
  armor = new DragonScale()

  override def performAttack: Int = {

    class FireBreath extends Weapon {
      var name = "Fire breath"
      var damage = (5,10)
      var id = "FIRE_BREATH"
      isDroppable = false
      attackBonus = 2

      override def getAttackText: String = {
        "opens its maw and releases a column of burning flames"
      }
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



