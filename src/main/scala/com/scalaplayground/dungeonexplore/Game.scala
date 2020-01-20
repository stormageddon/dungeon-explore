package com.scalaplayground.dungeonexplore.Game

import net.team2xh.scurses.{Colors, Scurses}

import scala.util.Random
import scala.collection.mutable._
import com.scalaplayground.dungeonexplore.Monster._
import com.scalaplayground.dungeonexplore.Weapon._
import com.scalaplayground.dungeonexplore.Armor._
import com.scalaplayground.dungeonexplore.constants.Constants._
import com.scalaplayground.dungeonexplore.constants.KeyboardCommands._
import com.scalaplayground.dungeonexplore.Item.Item
import com.scalaplayground.dungeonexplore.PathFinding.Dijkstra
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.Shrine._
import com.scalaplayground.dungeonexplore._

import scala.collection.mutable
import scala.io.Source

object Game extends App {
  def createPlayer: Player = {
    print("\033c")

    // try to read configuration first
    val filename = "config.txt"
    var name: String = ""
    var charClass: String = ""
    var charRace: String = ""

    try {
      for (line <- Source.fromFile(filename).getLines) {
        val configLine: Seq[String] = line.split(":")
        configLine(0) match {
          case "name" => name = configLine(1)
          case "class" => charClass = configLine(1)
          case "race" => charRace = configLine(1)
          case _ => println(s"Unknown config: ${configLine(0)}")
        }
      }
    }
    catch {
      case _: Throwable => println("No readable config file was found")
    }

    println("Hello, traveler.")
    if (name == null || name == None || name == "") {
      println("What is your name?")
      name = scala.io.StdIn.readLine(">> ")
    }

    if (charClass == null || charClass == None || charClass == "") {
      println("What path do you walk?")
      println("1. Barbarian")
      println("2. Cleric")
      println("3. Ranger")
      println("4. Rogue")
      println("5. Wizard")
      print(">> ")
      charClass = scala.io.StdIn.readInt() match {
        case 1 => "Barbarian"
        case 2 => "Cleric"
        case 3 => "Ranger"
        case 4 => "Rogue"
        case 5 => "Wizard"
        case _ => "Barbarian"
      }
    }

    if (charRace == null || charRace == None || charRace == "") {
      println("What is your heritage?")
      println("1. Dwarf")
      println("2. Elf")
      println("3. Halfling")
      println("4. Human")
      println("5. Lizardfolk")
      print(">> ")
      charRace = scala.io.StdIn.readInt() match {
        case 1 => "Dwarf"
        case 2 => "Elf"
        case 3 => "Halfling"
        case 4 => "Human"
        case 5 => "Lizardfolk"
        case _ => "Human"
      }
    }

    new Player(name, charClass, charRace)
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

  val gameState = new GameState(player, s)
  var isPlaying = true

  print("\033c")
  val colNum = NUM_COLS
  val rowNum = NUM_ROWS

  isPlaying = gameState.tick("")
  while(isPlaying) {

    if (player.health <= 0) {
      print("\033c")
      println("**********************************")
      println(s"${player.name} the Level ${player.level} ${player.charRace} ${player.charClass} was slain. RIP.")
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

class GameState(player:Player, screen: Scurses) {
  val dungeonHelper = new DungeonHelper
  val renderer = new Renderer(this, screen)
  var defeatedMonsters = Map[String,Int]()
  var tiles = mutable.Seq[mutable.Seq[Tile]]()
  var rooms: mutable.Seq[Room] = mutable.Seq[Room]()
  var shouldGenerateMonster = true
  var monsters: List[Monster] = List[Monster]()
  var monstersSlain = 0

  var droppedItems = List[Item]()
  var currTileDescription: String = "Nothing is here."
  var roundMessage: String = ""
  var monsterActionMessage: String = ""



  for (x <- 0 to NUM_COLS - 1) {
    tiles = tiles :+ Seq[Tile]()
    for (y <- 0 to NUM_ROWS - 1) {
      val thisPos = new Position(x, y)
      tiles(x) = tiles(x) :+ new EmptyTile(thisPos)
    }
  }

  def generateNeighbors = {
    tiles.map(row => {
      row.map(tile => {
        if (tile.passable) {
          getTileAtPosition(tile.position.x - 1, tile.position.y - 1) match {
            case Some(t) => {
              if (t.passable) {
                tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
              }
            }
            case None => Unit
          }

          getTileAtPosition(tile.position.x, tile.position.y - 1) match {
            case Some(t) => {
              if (t.passable) {
                tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
              }
            }
            case None => Unit
          }

          getTileAtPosition(tile.position.x + 1, tile.position.y - 1) match {
            case Some(t) => {
              if (t.passable) {
                tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
              }
            }
            case None => Unit
          }

          getTileAtPosition(tile.position.x - 1, tile.position.y) match {
            case Some(t) => {
              if (t.passable) {
                tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
              }
            }
            case None => Unit
          }

          getTileAtPosition(tile.position.x + 1, tile.position.y) match {
            case Some(t) => {
              if (t.passable) {
                tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
              }
            }
            case None => Unit
          }

          getTileAtPosition(tile.position.x - 1, tile.position.y + 1) match {
            case Some(t) => {
              if (t.passable) {
                tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
              }
            }
            case None => Unit
          }

          getTileAtPosition(tile.position.x, tile.position.y + 1) match {
            case Some(t) => {
              if (t.passable) {
                tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
              }
            }
            case None => Unit
          }

          getTileAtPosition(tile.position.x + 1, tile.position.y + 1) match {
            case Some(t) => {
              if (t.passable) {
                tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
              }
            }
            case None => Unit
          }
        }
      })
    })
  }

  def randomMap = {
    for (x <- 0 to NUM_COLS - 1) {
      tiles = tiles :+ Seq[Tile]()
      for (y <- 0 to NUM_ROWS - 1) {
        val thisPos = new Position(x, y)
        tiles(x) = tiles(x) :+ new EmptyTile(thisPos)
      }
    }


    // make rooms
    rooms = createRooms

    // build walls
    for (x <- 0 to NUM_COLS - 1) {
      for (y <- 0 to NUM_ROWS - 1) {
        if (getTileAtPosition(x, y).get.passable) {
          // North
          getTileAtPosition(x, y - 1) match {
            case Some(tile) => {
              if (!tile.passable) {
                tiles(x)(y - 1) = new HorizontalWall(new Position(x, y - 1))
              }
            }
            case None => ()
          }

          // South
          getTileAtPosition(x, y + 1) match {
            case Some(tile) => {
              if (!tile.passable) {
                tiles(x)(y + 1) = new HorizontalWall(new Position(x, y + 1))
              }
            }
            case None => ()
          }

          // East
          getTileAtPosition(x - 1, y) match {
            case Some(tile) => {
              if (!tile.passable) {
                tiles(x - 1)(y) = new VerticalWall(new Position(x - 1, y))
              }
            }
            case None => ()
          }

          // West
          getTileAtPosition(x + 1, y) match {
            case Some(tile) => {
              if (!tile.passable) {
                tiles(x + 1)(y) = new VerticalWall(new Position(x + 1, y))
              }
            }
            case None => ()
          }
        }
      }
    }

    tiles.map(row => {
      row.map(tile => {
        if (tile.passable) {
          getTileAtPosition(tile.position.x - 1, tile.position.y - 1) match {
            case Some(t) => {
              if (t.passable) {
                tile.neighbors = tile.neighbors :+ new Vertex(t, 2)
              }
            }
            case None => Unit
          }

          getTileAtPosition(tile.position.x, tile.position.y - 1) match {
            case Some(t) => {
              if (t.passable) {
                tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
              }
            }
            case None => Unit
          }

          getTileAtPosition(tile.position.x + 1, tile.position.y - 1) match {
            case Some(t) => {
              if (t.passable) {
                tile.neighbors = tile.neighbors :+ new Vertex(t, 2)
              }
            }
            case None => Unit
          }

          getTileAtPosition(tile.position.x - 1, tile.position.y) match {
            case Some(t) => {
              if (t.passable) {
                tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
              }
            }
            case None => Unit
          }

          getTileAtPosition(tile.position.x + 1, tile.position.y) match {
            case Some(t) => {
              if (t.passable) {
                tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
              }
            }
            case None => Unit
          }

          getTileAtPosition(tile.position.x - 1, tile.position.y + 1) match {
            case Some(t) => {
              if (t.passable) {
                tile.neighbors = tile.neighbors :+ new Vertex(t, 2)
              }
            }
            case None => Unit
          }

          getTileAtPosition(tile.position.x, tile.position.y + 1) match {
            case Some(t) => {
              if (t.passable) {
                tile.neighbors = tile.neighbors :+ new Vertex(t, 1)
              }
            }
            case None => Unit
          }

          getTileAtPosition(tile.position.x + 1, tile.position.y + 1) match {
            case Some(t) => {
              if (t.passable) {
                tile.neighbors = tile.neighbors :+ new Vertex(t, 2)
              }
            }
            case None => Unit
          }
        }
      })
    })
  }

  // setup map
  randomMap

  var shrine = generateShrine()

  monsters = generateMonster match {
    case Some(monster) => {
      monsters :+ monster
    }
    case None => monsters
  }


  def createRooms: mutable.Seq[Room] = {
    var listOfRooms: Seq[Room] = Seq[Room]()

    for (roomIterator <- 0 to MAX_NUM_ROOMS) {
      // Create a room
      val w = MIN_ROOM_WIDTH + Random.nextInt( MAX_ROOM_WIDTH - MIN_ROOM_WIDTH + 1)
      val h = MIN_ROOM_HEIGHT + Random.nextInt( MAX_ROOM_HEIGHT - MIN_ROOM_HEIGHT + 1)
      val randomPosition = new Position(Random.nextInt(NUM_COLS - w - 4), Random.nextInt(NUM_ROWS - h - 4))

      val newRoom = new Room(randomPosition, w, h)

      // validate the room
      val isValid = !listOfRooms.exists(room => room.intersects(newRoom))

      if (isValid) {
        // Tunnel out the room
        for (x <- newRoom.startPosition.x to newRoom.startPosition.x + newRoom.width) {
          for (y <- newRoom.startPosition.y to newRoom.startPosition.y + newRoom.height) {
            tiles(x)(y) = new FloorTile(new Position(x, y))
          }
        }

        if (listOfRooms.length == 0) {
          player.position = newRoom.getCenter
        }
        else {
          // Tunnel out a hallway to the previous room
          val prevCenter = listOfRooms(listOfRooms.length - 1).getCenter

          val x1 = newRoom.getCenter.x
          val y1 = newRoom.getCenter.y
          val x2 = prevCenter.x
          val y2 = prevCenter.y

          Random.nextInt(1) match {
            case 0 => {
              for (x <- Math.min(x1, x2) to Math.max(x1, x2)) {
                tiles(x)(y1) = new FloorTile(new Position(x, y1))
              }

              for (y <- Math.min(y1, y2) to Math.max(y1, y2)) {
                tiles(x2)(y) = new FloorTile(new Position(x2, y))
              }
            }
            case 1 => {
              for (y <- Math.min(y1, y2) to Math.max(y1, y2)) {
                tiles(x1)(y) = new FloorTile(new Position(x1, y))
              }

              for (x <- Math.min(x1, x2) to Math.max(x1, x2)) {
                tiles(x)(y2) = new FloorTile(new Position(x, y2))
              }
            }
          }



//          val firstRoomX = Math.min(newRoom.getCenter.x, prevCenter.x)
//          val firstRoomY = Math.min(newRoom.getCenter.y, prevCenter.y)
//          val secondRoomX = Math.max(newRoom.getCenter.x, prevCenter.x)
//          val secondRoomY = Math.max(newRoom.getCenter.y, prevCenter.y)
//
//          for (y <- firstRoomY to secondRoomY) {
//            tiles(firstRoomX)(y) = new FloorTile(new Position(firstRoomX, y))
//          }
//
//          for (x <- firstRoomX to secondRoomX) {
//            tiles(x)(newRoom.getCenter.y) = new FloorTile(new Position(x, secondRoomY))
//          }


//          Random.nextInt(1) match {
//            case 0 => {
//              for (x <- firstRoomX to secondRoomX) {
//                tiles(x)(firstRoomY) = new FloorTile(new Position(x, firstRoomY))
//              }
//
//              for (y <- firstRoomY to secondRoomY) {
//                tiles(prevCenter.x)(y) = new FloorTile(new Position(secondRoomX, y))
//              }
//            }
//            case 1 => {
//              for (y <- firstRoomY to secondRoomY) {
//                tiles(firstRoomX)(y) = new FloorTile(new Position(firstRoomX, y))
//              }
//
//              for (x <- firstRoomX to secondRoomX) {
//                tiles(x)(newRoom.getCenter.y) = new FloorTile(new Position(x, secondRoomY))
//              }
//            }
//          }

        }

        // Add room to the list
        listOfRooms = listOfRooms :+ newRoom
      }
    }

    listOfRooms
  }

  def generateShrine(): Shrine = {
    val randPos = if (rooms.length > 1) {
      rooms(Random.nextInt(rooms.length - 1)).getRandomPosition
    } else if (rooms.length > 0) {
      rooms(0).getRandomPosition
    } else {
      new Position(20,20)
    }

      Random.nextInt(100) match {
        case it if 0 until 50 contains it => return new HealthShrine(randPos)
        case it if 50 until 75 contains it => return new StrengthShrine(randPos)
        case it if 75 until 90 contains it => return new HolyShrine(randPos)
        case it if 90 until 100 contains it => return new CursedShrine(randPos)
      }
  }

  def generateMonster(): Option[Monster] = {
    if (!shouldGenerateMonster || (monsters != null && monsters.filter(m => m.isAlive).length >= MAX_MONSTERS_ALIVE)) {
      return None
    }
    if (monstersSlain >= 20) {
      monsterActionMessage = s"The door before you creaks open and an inhuman howl escapes from inside. A grayish light reveals the final resting place of Cem Hial...\n\n${monsterActionMessage}"
      shouldGenerateMonster = false
      val randPos = if (rooms.length > 1) {
        rooms(Random.nextInt(rooms.length - 1)).getRandomPosition
      } else if (rooms.length > 0) {
        rooms(0).getRandomPosition
      } else {
        // This really shouldn't happen
        new Position(20,20)
      }
      return Some(new CemHial(randPos))
    }
    else {
      val randPos = if (rooms.length > 1) {
        rooms(Random.nextInt(rooms.length - 1)).getRandomPosition
      } else if (rooms.length > 0) {
        rooms(0).getRandomPosition
      } else {
        // This really shouldn't happen
        new Position(20,20)
      }

      return Random.nextInt(100) match {
        case it if 0 until 25 contains it => Some(new Goblin(randPos))
        case it if 25 until 50 contains it => Some(new Kobold(randPos))
        case it if 50 until 60 contains it => Some(new GiantRat(randPos))
        case it if 60 until 75 contains it => Some(new Orc(randPos))
        case it if 75 until 81 contains it => Some(new Wolf(randPos))
        case it if 81 until 93 contains it => Some(new DireWolf(randPos))
        case it if 93 until 98 contains it => Some(new RockGolem(randPos))
        case it if 98 until 100 contains it => Some(new Dragon(randPos))
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
              val newItem = new Item(new Position(m.position.x, m.position.y), dispChar = "!", itemId = loot._1, hoverDescription = loot._2)
              droppedItems = droppedItems :+ newItem
              monsterActionMessage = monsterActionMessage + s"${m.name} dropped something with a loud clink.\n"
            }
            case None => None

          }
          monstersSlain += 1
          monsters = monsters.filter(monster => monster != m)
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
          val playerTile = getTileAtPosition(player.position.x, player.position.y)
          val newPos = monster.move(playerTile, tiles, getTileAtPosition(monster.position.x, monster.position.y))
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
          case None => ()
        }

      }
      case _ => ()
    }

    renderer.renderGameState
    renderer.renderStatsBar
    renderer.renderPlayerActions
    renderer.renderMonsterActions(monsterActionMessage)
    screen.refresh()
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
    if (x < 0 || x >= NUM_COLS || y < 0 || y >= NUM_ROWS) return None
    Option(tiles(x)(y))
  }




  /*
   * Commands are for debugging purposes
   */
  def runCommand = {
    val command = scala.io.StdIn.readLine.split(" ")
    command(0) match {
      case "spawn" => {
        if (command(1) != null) {
//          println("Spawning creature")
//          monsters = command(1) match {
//            case "orc" => monsters ++ List(new Orc())
//            case "wolf" => monsters ++ List(new Wolf())
//            case "cel" => monsters ++ List(new CemHial())
//            case "dragon" => monsters ++ List(new Dragon())
//            case _ => monsters
//          }
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
      case "n" => {
        val dijkstra = new Dijkstra
        val source = getTileAtPosition(command(1).toInt, command(2).toInt).get
        screen.put(0, NUM_ROWS, source.neighbors.toString)
        screen.put(0, NUM_ROWS + 1, dijkstra.findShortestPath(tiles.flatten, source, getTileAtPosition(player.position.x, player.position.y).get).toString)
      }
      case _ => println("Unknown command")
    }
  }
}
