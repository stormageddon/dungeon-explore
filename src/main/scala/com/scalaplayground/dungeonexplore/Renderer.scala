package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Game.GameState
import com.scalaplayground.dungeonexplore.constants.Constants._
import com.scalaplayground.dungeonexplore.Monster._
import com.scalaplayground.dungeonexplore.Shrine._
import com.scalaplayground.dungeonexplore.Item._

class Renderer(gs: GameState) {
  val gameState = gs

  def renderMonsterActions(monsterMessage:String) = {
    println(monsterMessage)
  }

  def renderPlayerActions() = {
    println(gameState.getPlayer.actionMessage)
  }

  def renderStatsBar = {
    val p = gameState.getPlayer()
    println(s"${p.name}")
    println(s"HP: ${p.health}    AC: ${p.armorClass + p.armor.armorBonus}     WIELDING: ${p.weapon.name} (${p.weapon.damage._1}-${p.weapon.damage._2} + ${p.weapon.attackBonus})     POTIONS: ${p.numPotions}")
    gameState.currTileDescription = "There is nothing here."
    gameState.droppedItems.map(item => {
      if (item.position.x == p.position.x && item.position.y == p.position.y) {
        gameState.currTileDescription = item.tileDescription
      }
    })
    val shrine = gameState.shrine
    if (shrine != null && shrine.position.x == p.position.x && shrine.position.y == p.position.y) {
      gameState.currTileDescription = shrine.tileDescription
    }
    println(s"${gameState.currTileDescription}")
    println(s"${gameState.roundMessage}")
    gameState.roundMessage = ""
  }

  def renderGameState: Unit = {
    val monsters: List[Monster] = gameState.monsters
    val shrine: Shrine = gameState.shrine
    val player: Player = gameState.getPlayer()
    val droppedItems: List[Item] = gameState.droppedItems

    for (y <- 0 to NUM_ROWS - 1) {
      for (x <- 0 to NUM_COLS - 1) {
        if (player.position.x == x && player.position.y == y) {
          player.render
        }
        else if (monsters.filter(m => m.position.x == x && m.position.y == y && m.isAlive).length > 0) {
          monsters.filter(m => m.position.x == x && m.position.y == y && m.isAlive).headOption match {
            case Some(monster) => print(monster.displayChar)
            case None => Unit
          }

        }
        else if (shrine != null && shrine.position.x == x && shrine.position.y == y) {
          shrine.render
        }
        else if ( droppedItems.filter(i => i.position.x == x && i.position.y == y).length > 0 ) {
          droppedItems.filter(i => i.position.x == x && i.position.y == y).headOption match {
            case Some(item) => {
              item.render
            }
            case None => Unit
          }
        }
        else {
          print(".")
        }
        print(" ")
      }
      y match {
        case 0 => println("    w,a,s,d - Move")
        case 1 => println("    q - Quaff a potion")
        case 2 => println("    u - Use item on ground")
        case _ => println("")
      }
    }
  }
}
