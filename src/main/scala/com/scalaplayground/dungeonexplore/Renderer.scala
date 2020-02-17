package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Game.GameState
import com.scalaplayground.dungeonexplore.constants.Constants._
import com.scalaplayground.dungeonexplore.Monster._
import com.scalaplayground.dungeonexplore.Shrine._
import com.scalaplayground.dungeonexplore.Item._
import net.team2xh.scurses.{Colors, Scurses}

import scala.collection.mutable

class Renderer(gs: GameState, screen: Scurses) {
  val BUILD_NUMBER = "0.4.20200208"

  val gameState = gs
  var sideContent = "EMPTY"
  val sideContentMap = Map[String, () => Unit](
    "EMPTY" -> (() => ()),
    "INVENTORY" -> renderInventoryScreen,
    "HELP_MENU" -> renderHelpScreen
  )

  def render = {
    renderGameState
    renderDebugBar
    renderStatsBar
    renderPlayerActions
    renderMonsterActions(gs.monsterActionMessages)
    screen.refresh
  }

  def renderDebugBar = {
    screen.put(NUM_COLS + 2, 0, s"Build: $BUILD_NUMBER", Colors.BRIGHT_BLACK)
  }

  def renderMonsterActions(monsterMessages:scala.collection.mutable.Map[String, Int]) = {
    monsterMessages.zipWithIndex.foreach( elem => {
      screen.put(0, NUM_ROWS + 7 + gameState.getPlayer.actionMessages.length + elem._2, elem._1._1, elem._1._2)
    })
  }

  def renderPlayerActions() = {
    var offset = 0
    gameState.getPlayer.actionMessages.foreach(message => {
      screen.put(0, NUM_ROWS + 6 + offset, s"$message ")
      offset = offset + 1
    })
  }

  def renderStatsBar = {
    val p = gameState.getPlayer()
    var offset = 2
    var descriptionTextColor = Colors.DIM_WHITE

    val characterString: String = s"${p.name}, the ${p.charRace} ${p.charClass} (Level ${p.level})"
    screen.put(0, NUM_ROWS + offset + 1, characterString)
    if (p.conditions.length > 0) {
      screen.put(characterString.length, NUM_ROWS + offset + 1, s" : ${p.conditions.head.name}", Colors.DIM_GREEN)
    }
    screen.put(0, NUM_ROWS + offset + 2, s"HP: ${p.health}/${p.maxHealth}    AC: ${p.armorClass + p.armor.armorBonus}     WIELDING: ${p.weapon.name} (${p.weapon.damage._1}-${p.weapon.damage._2} + ${p.weapon.attackBonus})")
    screen.put(0, NUM_ROWS + offset + 3, s"Dungeon level: ${gameState.dungeonLevel}")
    gameState.currTileDescription = "There is nothing here."
    gameState.getFloorItems.map(item => {
      if (item.position.x == p.position.x && item.position.y == p.position.y) {
        gameState.currTileDescription = item.tileDescription
        descriptionTextColor = if (!item.identified) Colors.BRIGHT_BLUE else Colors.DIM_WHITE
      }
    })
    val shrine = gameState.shrine
    if (shrine != null && shrine.position.x == p.position.x && shrine.position.y == p.position.y) {
      gameState.currTileDescription = shrine.tileDescription
    }
    screen.put(0, NUM_ROWS + offset + 4, s"${gameState.currTileDescription}", descriptionTextColor)
    screen.put(0, NUM_ROWS + offset + 5, s"${gameState.roundMessage}")
    gameState.roundMessage = ""
  }

  def renderGameState: Unit = {
    val monsters: Seq[Monster] = gameState.getMonsters
    val shrine: Shrine = gameState.shrine
    val player: Player = gameState.getPlayer()
    val droppedItems: Seq[Item] = gameState.getFloorItems
    val traps = gameState.getTraps
    val tiles: mutable.Seq[mutable.Seq[Tile]] = gameState.tiles

    for (x <- 0 to NUM_COLS - 1) {
      for (y <- 0 to NUM_ROWS - 1) {
        if (player.position.x == x && player.position.y == y) {
          player.render(screen)
        }
        else if (monsters.filter(m => m.position.x == x && m.position.y == y && m.isAlive).length > 0) {
          monsters.filter(m => m.position.x == x && m.position.y == y && m.isAlive).headOption match {
            case Some(monster) => {
              if (gameState.getTileAtPosition(x, y).get.currentlyVisible) {
                val color = if (monster.conditions.nonEmpty) Colors.DIM_GREEN else Colors.DIM_WHITE
                screen.put(x, y, monster.displayChar, color)
              }
              else {
                screen.put(x, y, " ") // hide monster if it's not visible
              }
            }
            case None => Unit
          }
        }
        else if (shrine != null && shrine.position.x == x && shrine.position.y == y && gameState.getTileAtPosition(shrine.position.x, shrine.position.y).get.currentlyVisible) {
          shrine.render(x, y, screen)
        }
        else if ( droppedItems.filter(i => i.position.x == x && i.position.y == y).length > 0 ) {
          droppedItems.filter(i => i.position.x == x && i.position.y == y).headOption match {
            case Some(item) => {
              if (gameState.getTileAtPosition(item.position.x, item.position.y).get.currentlyVisible) {
                item.render(x, y, screen)
              }
            }
            case None => Unit
          }
        }
        else if (traps.find(t => t.pos.x == x && t.pos.y == y && t.identified).isDefined) {
          val trap = traps.find(t => t.pos.x == x && t.pos.y == y).get
          screen.put(x, y, trap.displayChar, Colors.DIM_RED)
        }
        else {
          val t = tiles(x)(y)

          if (t.hasBeenSeen && t.currentlyVisible) {
            screen.put(x, y, t.displayChar)
          }
          else if (t.hasBeenSeen && !t.currentlyVisible) {
            screen.put(x, y, t.displayChar, Colors.BRIGHT_BLACK)
          }
          else {
            screen.put(x, y, " ")
          }
        }
      }

      renderRightPanel(sideContentMap(sideContent))
    }
  }

  def renderHelpScreen(): Unit = {
    for (x <- 0 to NUM_COLS - 1) {
      x match {
        case 0 => screen.put(NUM_COLS + 1, 1, "w,a,s,d - Move")
        case 1 => screen.put(NUM_COLS + 1, 2, "q,e,z,x - Move diagonally")
        case 2 => screen.put(NUM_COLS + 1, 3, "c - (c)onsume a potion")
        case 3 => screen.put(NUM_COLS + 1, 4, "u - (u)se item on ground")
        case 4 => screen.put(NUM_COLS + 1, 5, "E - (E)quip item")
        case 5 => screen.put(NUM_COLS + 1, 6, "i - display (i)nventory")
        case 6 => screen.put(NUM_COLS + 1, 7, "h - display (h)elp menu")
        case 7 => screen.put(NUM_COLS + 1, 8, "ESC - quit")
        case _ => Unit
      }
    }
  }

  def renderInventoryScreen(): Unit = {
    var index = 0
    gs.getPlayer.inventory.getItems.foreach(itemMapElement => {
      val item = itemMapElement._2.head
      val color = if (item.identified)  (if (!item.enchanted) Colors.DIM_WHITE else Colors.DIM_GREEN) else Colors.BRIGHT_BLUE
      screen.put(NUM_COLS + 1, index + 1, s"${index + 1}: ${item.name} x${itemMapElement._2.size}", color)
      index = index + 1
    })
  }

  def renderRightPanel(callback: () => Unit) = {
    callback()
  }
}

