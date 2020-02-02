package com.scalaplayground.dungeonexplore.Floor


import com.scalaplayground.dungeonexplore.Armor.Armor
import com.scalaplayground.dungeonexplore.Item.Item
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.Room
import com.scalaplayground.dungeonexplore.Weapons.{BlessedWeaponDecorator, CursedWeaponDecorator, FlamingWeaponDecorator, Weapon}

import scala.util.Random

case class Floor(val level: Int) {
  var rooms: Seq[Room] = Seq[Room]()
  var droppedItems: Seq[Item] = Seq[Item]()

  def populate = {
    rooms.foreach(room => {
      // populate items
      val numberOfItemsInRoom = 3 // TODO: Randomize
      for (i <- 0 to numberOfItemsInRoom) {
        val pos = room.getRandomValidPosition
        val item: Item = generateRandomItem(pos)
        droppedItems = droppedItems :+ item
      }

      // populate mobs
    })
  }

  def generateRandomItem(pos: Position): Item = {
    level match {
      case _ => {
        Random.nextInt(100) match {
          case roll if 0 until 33 contains roll => {
            new Item(pos, "!", "A swirling potion lies here", "POTION")
          }
          case roll if 33 until 66 contains roll => {
            println(s"GENERATE WEAPON AT ${pos.toString}")
            Random.nextInt(100) match {
              case r if 0 until 10 contains r => {
                generateMagicItem(pos)
              }
              case _ => {
                val weapon = Weapon.generateWeapon
                weapon.position = pos
                weapon
              }
            }

          }
          case roll if 66 until 100 contains roll => {
            println(s"GENERATE ARMOR AT ${pos.toString}")
            val armor = Armor.generateArmor
            armor.position = pos
            armor
          }
        }
      }
      //case _ => new Item(pos, "!", "A swirling potion lies here", "POTION")
    }
  }

  def generateMagicItem(position: Position): Item = {
    Random.nextInt(100) match {
      case it if 0 until 33 contains it => {
        val weapon: Weapon = new FlamingWeaponDecorator(Weapon.generateWeapon)
        weapon.position = position
        weapon
      }
      case it if 33 until 66 contains it => {
        val weapon: Weapon = new BlessedWeaponDecorator(Weapon.generateWeapon)
        weapon.position = position
        weapon
      }
      case it if 66 until 100 contains it => {
        val weapon: Weapon = new CursedWeaponDecorator(Weapon.generateWeapon)
        weapon.position = position
        weapon
      }
    }
  }

}
