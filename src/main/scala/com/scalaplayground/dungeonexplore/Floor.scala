package com.scalaplayground.dungeonexplore.Floor


import com.scalaplayground.dungeonexplore.Armor.{Armor, Chain, Leather, PlateMail}
import com.scalaplayground.dungeonexplore.Consumables.{HealthPotion, Potion, Scroll}
import com.scalaplayground.dungeonexplore.Item.Item
import com.scalaplayground.dungeonexplore.Monster._
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.{DartTrap, Room, Trap}
import com.scalaplayground.dungeonexplore.Weapons._

import scala.util.Random

case class Floor(val level: Int, val bossLevel: Boolean = false) {
  var rooms = Seq[Room]()
  var droppedItems = Seq[Item]()
  var monsters = Seq[Monster]()
  var traps = Seq[Trap]()

  def populate: Unit = {
    if (bossLevel) {
      generateBossMonster
      return ()
    }
    rooms.zipWithIndex.foreach(elem => {
      val room = elem._1
      // populate items
      if (Random.nextInt(100) < 25) {
        val numberOfItemsInRoom = Random.nextInt(3) + 1
        for (i <- 0 to numberOfItemsInRoom) {
          val pos = room.getRandomValidPosition
          val item: Item = generateRandomItem(pos)
          droppedItems = droppedItems :+ item
        }
      }


      // populate mobs, unless it is start room
      if (elem._2 != 0) {
        for (i <- 0 to Random.nextInt(3)) {
          monsters = if (monsters == null) List[Monster]() else monsters
          val randPos = room.getRandomValidPosition
          generateMonster(new Position(randPos.y, randPos.x)) match {
            case Some(monster) => monsters = monsters :+ monster
            case None => ()
          }
        }
      }

      // populate traps
      if (Random.nextInt(100) < 15) {
        val randPos = room.getRandomValidPosition
        traps = traps :+ new DartTrap(randPos)
      }
    })
  }

  def getMonsters: Seq[Monster] = {
    return monsters
  }

  def getRandomRoom: Room = {
    rooms(Random.nextInt(rooms.length))
  }

  def generateRandomItem(pos: Position): Item = {
//    return Scroll.generateScroll(pos)
//    return Potion.generatePotion(pos)

    level match {
      case 0 | 1 | 2 | 3 => {
        Random.nextInt(100) match {
          case roll if 0 until 33 contains roll => {
            Potion.generatePotion(pos)
          }
          case roll if 33 until 53 contains roll => {
            Scroll.generateScroll(pos)
          }
          case roll if 53 until 66 contains roll => {
            Random.nextInt(100) match {
              case r if 0 until 10 contains r => {
                generateMagicItem(pos)
              }
              case _ => {
                Random.nextInt(100) match {
                  case randomWeapon if 0 until 10 contains randomWeapon => {
                    val weapon = new GreatAxe
                    weapon.position = pos
                    weapon
                  }
                  case _ => {
                    val possibleWeapons = List[Weapon](
                      new Dagger,
                      new ShortSword,
                      new Spear
                    )
                    val weapon = possibleWeapons(Random.nextInt(possibleWeapons.size))
                    weapon.position = pos
                    weapon
                  }
                }
              }
            }
          }
          case roll if 66 until 98 contains roll => {
            Random.nextInt(100) match {
              case r if 0 until 80 contains r => {
                val armor: Leather = new Leather
                armor.position = pos
                armor
              }
              case r if 80 until 98 contains r => {
                val armor = new Chain
                armor.position = pos
                armor
              }
              case r if 98 until 100 contains r => {
                val armor = new PlateMail
                armor.position = pos
                armor
              }
            }
          }
          case roll if 98 until 100 contains roll => {
            new Item(pos, "!", "A shiny ring with a glowing ruby", "RING_OF_HEALTH", name = "Ring of health")
          }
        }
      }
      case _ => new HealthPotion(pos)
    }
  }

  def generateMagicItem(position: Position): Item = {
    Random.nextInt(100) match {
      case it if 0 until 30 contains it => {
        val weapon: Weapon = new PoisonedWeaponDecorator(Weapon.generateWeapon)
        weapon.position = position
        weapon
      }
      case it if 30 until 45 contains it => {
        val weapon: Weapon = new FlamingWeaponDecorator(Weapon.generateWeapon)
        weapon.position = position
        weapon
      }
      case it if 45 until 70 contains it => {
        val weapon: Weapon = new BlessedWeaponDecorator(Weapon.generateWeapon)
        weapon.position = position
        weapon
      }
      case it if 70 until 95 contains it => {
        val weapon: Weapon = new CursedWeaponDecorator(Weapon.generateWeapon)
        weapon.position = position
        weapon
      }
      case it if 95 until 100 contains it => {
        val weapon: Weapon = new VampiricDecorator(Weapon.generateWeapon)
        weapon.position = position
        weapon
      }
    }
  }

  def generateBossMonster = {
    if (level == 4) {

      val startPos = rooms.head.getRandomValidPosition
      val cemHial = new CemHial(startPos)

      monsters = Seq[Monster](cemHial)
    }
  }

  def generateMonster(pos:Position): Option[Monster] = {

    level match {
      case 0 | 1 | 2 => {
        val monster = Random.nextInt(100) match {
          case roll if 0 until 25 contains roll => {
            new GiantRat(pos)
          }
          case roll if 25 until 50 contains roll => {
            new Kobold(pos)
          }
          case roll if 50 until 75 contains roll => {
            new Goblin(pos)
          }
          case roll if 75 until 100 contains roll => {
            new Wolf(pos)
          }
        }
        Some(monster)
      }
      case 3 => {
        val monster = Random.nextInt(100) match {
          case roll if 0 until 10 contains roll => {
            new GiantRat(pos)
          }
          case roll if 10 until 35 contains roll => {
            new Kobold(pos)
          }
          case roll if 35 until 60 contains roll => {
            new Goblin(pos)
          }
          case roll if 60 until 85 contains roll => {
            new Orc(pos)
          }
          case roll if 85 until 95 contains roll => {
            new DireWolf(pos)
          }
          case roll if 95 until 100 contains roll => {
            new RockGolem(pos)
          }
        }
        Some(monster)
      }
      case _ => None
    }
  }
}
