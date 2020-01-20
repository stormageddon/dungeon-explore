package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Game.GameState
import com.scalaplayground.dungeonexplore.constants.Constants._
import com.scalaplayground.dungeonexplore.Monster._
import com.scalaplayground.dungeonexplore.Shrine._
import com.scalaplayground.dungeonexplore.Item._
import net.team2xh.scurses.Scurses

import scala.collection.mutable

class Renderer(gs: GameState, screen: Scurses) {
  val gameState = gs
  val dungeonHelper = new DungeonHelper

  def renderMonsterActions(monsterMessage:String) = {
    screen.put(0, NUM_ROWS + 6, monsterMessage)
  }

  def renderPlayerActions() = {
    screen.put(0, NUM_ROWS + 5, gameState.getPlayer.actionMessage)
  }

  def renderStatsBar = {
    val p = gameState.getPlayer()
    var offset = 2

    screen.put(0, NUM_ROWS + offset + 1, s"${p.name}, the ${p.charRace} ${p.charClass} (Level ${p.level})")
    screen.put(0, NUM_ROWS + offset + 2, s"HP: ${p.health}    AC: ${p.armorClass + p.armor.armorBonus}     WIELDING: ${p.weapon.name} (${p.weapon.damage._1}-${p.weapon.damage._2} + ${p.weapon.attackBonus})     POTIONS: ${p.numPotions}")
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
    screen.put(0, NUM_ROWS + offset + 3, s"${gameState.currTileDescription}")
    screen.put(0, NUM_ROWS + offset + 4, s"${gameState.roundMessage}")
    gameState.roundMessage = ""
  }

  def renderGameState: Unit = {
    val monsters: List[Monster] = gameState.monsters
    val shrine: Shrine = gameState.shrine
    val player: Player = gameState.getPlayer()
    val droppedItems: List[Item] = gameState.droppedItems
    val tiles: mutable.Seq[mutable.Seq[Tile]] = gameState.tiles

    for (x <- 0 to NUM_COLS - 1) {
      for (y <- 0 to NUM_ROWS - 1) {
        if (player.position.x == x && player.position.y == y) {
          player.render(screen)
        }
        else if (monsters.filter(m => m.position.x == x && m.position.y == y && m.isAlive).length > 0) {
          monsters.filter(m => m.position.x == x && m.position.y == y && m.isAlive).headOption match {
            case Some(monster) => screen.put(x, y, monster.displayChar)//dungeonHelper.padGameObjectChar(monster.displayChar))
            case None => Unit
          }
        }
        else if (shrine != null && shrine.position.x == x && shrine.position.y == y) {
          shrine.render(x, y, screen)
        }
        else if ( droppedItems.filter(i => i.position.x == x && i.position.y == y).length > 0 ) {
          droppedItems.filter(i => i.position.x == x && i.position.y == y).headOption match {
            case Some(item) => {
              item.render(x, y, screen)
            }
            case None => Unit
          }
        }
        else {
          // find the tile that matches this position
          //val t = tiles.filter(tile => tile.position.x == x && tile.position.y == y).head
          val t = tiles(x)(y)
          screen.put(x, y, t.displayChar)
        }
      }

      x match {
        case 0 => screen.put(NUM_COLS + 1, 0, "    w,a,s,d - Move")
        case 1 => screen.put(NUM_COLS + 1, 1, "    q,e,z,x - Move diagonally")
        case 2 => screen.put(NUM_COLS + 1, 2, "    r - Quaff a potion")
        case 3 => screen.put(NUM_COLS + 1, 3, "    u - Use item on ground")
        case 4 => screen.put(NUM_COLS + 1, 4, "    ESC - quit")
        case _ => Unit
      }
    }
  }
}
