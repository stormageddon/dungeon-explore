package com.scalaplayground.dungeonexplore.Monster

import com.scalaplayground.dungeonexplore.Armor._
import com.scalaplayground.dungeonexplore.Item.Item

import scala.util.Random
import com.scalaplayground.dungeonexplore.constants.Constants._
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.{Tile}
import com.scalaplayground.dungeonexplore.PathFinding.AStar
import com.scalaplayground.dungeonexplore.Weapons._

abstract class Monster {
  val name: String
  var health: Int
  var armorClass: Int = 8
  var attackBonus: Int = 1
  var weapon: Weapon = new RustyWeaponDecorator(new Dagger)
  var armor: Armor = new Natural
  var position: Position = new Position(Random.nextInt(NUM_ROWS), Random.nextInt(NUM_COLS))
  var displayChar: String = "m"
  val canAvoidObstacles = false

  def isAlive: Boolean = {
    Armor.generateArmor
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

  def dropLoot: Option[Item] = {
    val roll = Random.nextInt(100)
    if (roll <= MAGIC_ITEM_DROP_PERCENTAGE) {
      return Some(generateMagicItem)
    }
    else if (roll <= weapon.dropChance && weapon.isDroppable) {
      weapon.position = position
      return Some(weapon)
    }
    else if (roll <= armor.dropChance && armor.isDroppable) {
      return Some(new Item(position, "!", armor.name, armor.id))
    }
    else if (roll <= POTION_DROP_PERCENTAGE) {
      return Some(new Item(position, "!", "A swirling potion lies here", "POTION", name = "Red potion"))
    }
    None
  }

  def generateMagicItem: Item = {
    Random.nextInt(100) match {
      case it if 0 until 15 contains it => {
        val weapon: Weapon = new FlamingWeaponDecorator(Weapon.generateWeapon)
        weapon.position = position
        weapon
      }
      case it if 15 until 75 contains it => {
        val weapon: Weapon = new BlessedWeaponDecorator(Weapon.generateWeapon)
        weapon.position = position
        weapon
      }
      case it if 75 until 100 contains it => {
        val weapon: Weapon = new CursedWeaponDecorator(Weapon.generateWeapon)
        weapon.position = position
        weapon
      }
    }
  }

  def move(target: Option[Tile], tiles: Seq[Seq[Tile]], currTile: Option[Tile]): Position = {
    val aStar = new AStar

    val nextMove: Option[Tile] = target match {
      case Some(targetTile) => {
        currTile match {
          case Some(currentTile) => {
            // check if monster is within range to notice
            if (Math.abs(currentTile.position.x - targetTile.position.x) + Math.abs(currentTile.position.y - targetTile.position.y) > 5) {
              // don't move
              return currentTile.position
            }

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
  weapon = List(new RustyWeaponDecorator(new Dagger),
    new Dagger,
    new FineWeaponDecorator(new Dagger)
  )(Random.nextInt(3))
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
    new RustyWeaponDecorator(new ShortSword),
    new ShortSword,
    new RustyWeaponDecorator(new Dagger),
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
    new RustyWeaponDecorator(new ShortSword),
    new ShortSword,
    new FineWeaponDecorator(new ShortSword),
    new RustyWeaponDecorator(new GreatAxe),
    new GreatAxe,
    new FineWeaponDecorator(new GreatAxe)
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
      name = "Fire breath"
      var damage = (5,10)
      id = "FIRE_BREATH"
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



