package com.scalaplayground.dungeonexplore.Game

import net.team2xh.scurses.{Colors, Scurses}

import scala.util.Random
import com.scalaplayground.dungeonexplore.Monster._
import com.scalaplayground.dungeonexplore.Weapon._
import com.scalaplayground.dungeonexplore.Armor._
import com.scalaplayground.dungeonexplore.constants.Constants._
import com.scalaplayground.dungeonexplore.constants.KeyboardCommands._
import com.scalaplayground.dungeonexplore.Item.Item
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.Shrine._
import com.scalaplayground.dungeonexplore._

object Game extends App {
  def createPlayer: Player = {
    println("What is your name, traveler?")
    new Player(scala.io.StdIn.readLine(">> "))
  }

  var s: Scurses = new Scurses

  Scurses { screen =>
    // The screen will only be in Scurses mode inside this block
    // Scurses will reset the terminal buffer and everything when outside

    // Get the current terminal size
    val (w, h) = screen.size

    val greeting = "THE TOMB OF CEM HIAL"
    val prompt = "Press a key to continue..."
    // Put some strings in the middle of the screen
    screen.put(w/2 - greeting.length/2, h/2, greeting)
    screen.put(w/2 - prompt.length/2, h/2 + 1, prompt, Colors.BRIGHT_BLACK)
    // Flush the buffer
    screen.refresh()
    s = screen
    // Wait for an input without storing it
    screen.keypress()
  }

  s.refresh()
  val player = createPlayer

  val gameState = new GameState(player)
  var isPlaying = true

  print("\033c")
  val colNum = NUM_COLS
  val rowNum = NUM_ROWS

  isPlaying = gameState.tick("")
  while(isPlaying) {

    if (player.health <= 0) {
      print("\033c")
      println("**********************************")
      println(s"${player.name} was slain. RIP.")
      isPlaying = false
    }
    else {

      print(">> ")

      val input = scala.io.StdIn.readLine.slice(0,1)
      val key = input.toString.asInstanceOf[String]

      if (key == "ESC") {
        isPlaying = false
      }
      else {
        isPlaying = gameState.tick(key)
      }
    }
  }

  // End Game state
  println(s"${gameState.monstersSlain} monsters were defeated. Nice job!")
  gameState.defeatedMonsters.keys.map(monsterType => println(s"${monsterType}'s killed: ${gameState.defeatedMonsters.get(monsterType).get}"))
  println("======== Ending gear ========")
  println(s"Weapon: ${player.weapon.name} (${player.weapon.damage._1}-${player.weapon.damage._2}, ${player.weapon.attackBonus})")
  println(s"Armor: ${player.armor.name} (${player.armor.armorBonus})")
  println("**********************************")
}

class GameState(player:Player) {
  val dungeonHelper = new DungeonHelper
  val renderer = new Renderer(this)
  var defeatedMonsters = Map[String,Int]()
  var shouldGenerateMonster = true
  var monsters: List[Monster] = List[Monster](generateMonster().get)
  var monstersSlain = 0
  var shrine = generateShrine()
  var droppedItems = List[Item]()
  var currTileDescription: String = "Nothing is here."
  var roundMessage: String = ""
  var monsterActionMessage: String = ""
  var tiles = Seq[Tile]()


  def randomMap = {
    for (y <- 0 to NUM_ROWS - 1) {
      for (x <- 0 to NUM_COLS - 1) {
        val thisPos = new Position(x, y)
        if (Random.nextInt(100) <= 15) {
          tiles = tiles :+ List(new HorizontalWall(thisPos), new VerticalWall(thisPos))(Random.nextInt(2))
        }
        else {
          tiles = tiles :+ new FloorTile(thisPos)
        }
      }
    }
  }

  // setup map
  randomMap

  def generateShrine(): Shrine = {
      Random.nextInt(100) match {
        case it if 0 until 50 contains it => return new HealthShrine(new Position(Random.nextInt(20),Random.nextInt(20)))
        case it if 50 until 75 contains it => return new StrengthShrine(new Position(Random.nextInt(20),Random.nextInt(20)))
        case it if 75 until 90 contains it => return new HolyShrine(new Position(Random.nextInt(20),Random.nextInt(20)))
        case it if 90 until 100 contains it => return new CursedShrine(new Position(Random.nextInt(20),Random.nextInt(20)))
      }
  }

  def generateMonster(): Option[Monster] = {
    if (!shouldGenerateMonster || (monsters != null && monsters.filter(m => m.isAlive).length >= MAX_MONSTERS_ALIVE)) {
      return None
    }
    if (monstersSlain >= 20) {

      monsterActionMessage = s"The door before you creaks open and an inhuman howl escapes from inside. A grayish light reveals the final resting place of Cem Hial...\n\n${monsterActionMessage}"
      shouldGenerateMonster = false
      return Some(new CemHial())
    }
    else {
      return Random.nextInt(100) match {
        case it if 0 until 25 contains it => Some(new Goblin())
        case it if 25 until 50 contains it => Some(new Kobold())
        case it if 50 until 60 contains it => Some(new GiantRat())
        case it if 60 until 75 contains it => Some(new Orc())
        case it if 75 until 81 contains it => Some(new Wolf())
        case it if 81 until 93 contains it => Some(new DireWolf())
        case it if 93 until 98 contains it => Some(new RockGolem())
        case it if 98 until 100 contains it => Some(new Dragon())
      }
    }
  }

  def performPlayerAttack(monster:Monster) = {
    playerHasValidTarget(player, monster) match {
      case Some(m) => {
        val attack = player.performAttack(m.armorClass)
        if (attack >= m.armorClass) {
          val damage = player.weapon.attack
          player.actionMessage = player.actionMessage + s"You stab with your ${player.weapon.name} dealing ${damage} damage!\n"
          m.health = m.health - damage
        }
        if (m.health <= 0) {
          monsterActionMessage = s"${monsterActionMessage}${m.name} was slain!\n"
          m.dropLoot match {
            case Some(loot) => {
              var newItem = new Item(new Position(m.position.x, m.position.y), dispChar = "!", itemId = loot._1, hoverDescription = loot._2)
              droppedItems = droppedItems :+ newItem
              monsterActionMessage = monsterActionMessage + s"${m.name} dropped something with a loud clink.\n"
            }
            case None => None

          }
          monstersSlain += 1
          generateMonster match {
            case Some(monster) => monsters = monsters ++ List[Monster](monster)
            case None => Unit
          }
        }
      }
      case None => Unit
    }
  }

  def performPlayerMove(xVel: Int, yVel: Int): Boolean = {
    val newPos = player.move(xVel, yVel)
    getMonsterAtPosition(newPos) match {
      case Some(monster) => {
        performPlayerAttack(monster)
        return false
      }
      case None => {
        player.position = getTileAtPosition(newPos.x, newPos.y) match {
          case Some(tile) => if (tile.passable) newPos else player.position
          case None => newPos
        }
        return true
      }
    }
  }

  def tick(action: String): Boolean = {
    print("\033c")

    var playerDidMove = false
    val playerIsAlive = true


    playerDidMove = action match {
      case QUAFF_POTION => player.quaffPotion
      case MOVE_UP => performPlayerMove(0, -1)
      case MOVE_LEFT => performPlayerMove(-1, 0)
      case MOVE_DOWN => performPlayerMove(0, 1)
      case MOVE_RIGHT => performPlayerMove(1, 0)
      case MOVE_UP_LEFT => performPlayerMove(-1, -1)
      case MOVE_UP_RIGHT => performPlayerMove(1, -1)
      case MOVE_DOWN_LEFT => performPlayerMove(-1, 1)
      case MOVE_DOWN_RIGHT => performPlayerMove(1, 1)
      case USE_ITEM => {
        // check for items
        droppedItems.filter(item => item.position.x == player.position.x && item.position.y == player.position.y).headOption match {
          case Some(item) => {
            item.interact(player)
            droppedItems = droppedItems.filterNot( i => i == item)
            if(item.id == "NIGHT_BLADE") {
              renderer.renderPlayerActions
              renderer.renderMonsterActions(monsterActionMessage)
              return false
            }
          }
          case _ => {
            if (shrine.position.x == player.position.x && shrine.position.y == player.position.y) {
              roundMessage = shrine.interact(player)
            } else {
              print("There is nothing to use here.")
            }
          }
        }
        false
      }
      case RUN_COMMAND => {
        runCommand
        false
      }
      case ESCAPE => {
        // quit the game
        false
      }
      case _ => false
    }

    if (monsters.length == 0) {
      return playerIsAlive
    }

    // Perform monster actions
    monsters.map(monster => {
      if (monster.isAlive) {
        if (monsterHasValidTarget(monster, player) && monster.isAlive) {
          val hitRoll = monster.performAttack
          monsterActionMessage = monsterActionMessage + s"attack roll of ${hitRoll} vs AC ${player.armorClass + player.armor.armorBonus}\n"
          if (hitRoll >= player.armorClass + player.armor.armorBonus) {
            val damage = monster.calculateDamage
            monsterActionMessage = monsterActionMessage + s"${monster.name} ${monster.weapon.getAttackText} dealing ${damage} damage\n"
            player.health = player.health - damage
          }
          else {
            monsterActionMessage = monsterActionMessage + s"The ${monster.name} missed you.\n"
          }
        }

        if (monster.isAlive) {
          val newPos = monster.move(Some(player.position))
          getTileAtPosition(newPos.x, newPos.y) match {
            case Some(tile) => {
              if (tile.passable || monster.canAvoidObstacles) {
                monster.position = newPos
              } else {
                // get random (legal) position
                monster.position = monster.position
              }
            }
            case None => newPos
          }
        }
      }
    })

    // generate new enemy?
    Random.nextInt(100) match {
      case it if 0 until 5 contains it => {
        generateMonster match {
          case Some(monster) => monsters = monsters ++ List[Monster](monster)
          case None => Unit
        }

      }
      case _ => Unit
    }

    renderer.renderGameState
    renderer.renderStatsBar
    renderer.renderPlayerActions
    renderer.renderMonsterActions(monsterActionMessage)

    player.endRound
    monsterActionMessage = ""

    return playerIsAlive
  }

  def playerHasValidTarget(player: Player, monster: Monster): Option[Monster] = {
    if (player == null || monster == null) {
      return None
    }
    if ((math.abs(player.position.x - monster.position.x) <= 1) && (math.abs(player.position.y - monster.position.y) <= 1)) {
      return Some(monster)
    }
    None
  }

  def monsterHasValidTarget(monster: Monster, player: Player): Boolean = {
    if ((math.abs(monster.position.x - this.player.position.x) <= 1) && (math.abs(monster.position.y - player.position.y) <= 1)) {
      return true
    }
    false
  }

  def getPlayer(): Player = {
    return player
  }

  def getMonsterAtPosition(position:Position): Option[Monster] = {
    monsters.filter(monster => monster.isAlive && monster.position.x == position.x && monster.position.y == position.y).headOption
  }

  def getTileAtPosition(x: Int, y: Int): Option[Tile] = {
    tiles.filter(tile => tile.position.x == x && tile.position.y == y).headOption
  }




  /*
   * Commands are for debugging purposes
   */
  def runCommand = {
    val command = scala.io.StdIn.readLine.split(" ")
    command(0) match {
      case "spawn" => {
        if (command(1) != null) {
          println("Spawning creature")
          monsters = command(1) match {
            case "orc" => monsters ++ List(new Orc())
            case "wolf" => monsters ++ List(new Wolf())
            case "cel" => monsters ++ List(new CemHial())
            case "dragon" => monsters ++ List(new Dragon())
            case _ => monsters
          }
        }
      }
      case "give" => {
        if (command(1) != null) {
          command(1) match {
            case "axe" => player.weapon = new FineGreatAxe()
            case "night_blade" => player.weapon = new NightBlade()
            case "dagger" => player.weapon = new FineDagger()
            case "sword" => player.weapon = new FineShortSword()
            case "spear" => player.weapon = new Spear()
            case "dragon_scale" => player.armor = new DragonScale()
          }
        }
      }
      case "heal" => {
        player.health = player.maxHealth
      }
      case _ => println("Unknown command")
    }
  }
}
