package com.scalaplayground.dungeonexplore.Game

import net.team2xh.scurses.{Colors, Scurses}

import scala.util.Random
import scala.collection.mutable._
import com.scalaplayground.dungeonexplore.Monster._
import com.scalaplayground.dungeonexplore.Armor._
import com.scalaplayground.dungeonexplore.Consumables.{Consumable, IdentifyScroll, Potion, Scroll}
import com.scalaplayground.dungeonexplore.Floor.Floor
import com.scalaplayground.dungeonexplore.constants.Constants._
import com.scalaplayground.dungeonexplore.constants.KeyboardCommands._
import com.scalaplayground.dungeonexplore.Item.Item
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.Shrine._
import com.scalaplayground.dungeonexplore.Weapons._
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

  s.hideCursor
  s.refresh()
  val player = createPlayer

  val gameState = new GameState(player, s)
  var isPlaying = true

  Potion.initializePotions
  Scroll.initializeScrolls(gameState)

  print("\033c")
  val colNum = NUM_COLS
  val rowNum = NUM_ROWS

  isPlaying = gameState.tick(0)
  while(isPlaying) {

    if (player.health <= 0) {
      print("\033c")
      println("**********************************")
      println(s"${player.name} the Level ${player.level} ${player.charRace} ${player.charClass} was slain. RIP.")
      isPlaying = false
    }
    else {

      print(">> ")
      val setToRaw = Array[String]("/bin/sh", "-c", "stty raw </dev/tty")
      val setToCooked = Array[String]("/bin/sh", "-c", "stty cooked </dev/tty")
      Runtime.getRuntime.exec(setToRaw).waitFor()
      val input = Console.in.read
      println(s"input: $input")

      Runtime.getRuntime.exec(setToCooked).waitFor()

      // 27 == escape
      if (input == 27) {
        isPlaying = false
      }
      else {
        isPlaying = gameState.tick(input)
      }
    }
  }

  // End Game state
  println(s"${gameState.monstersSlain} monsters were defeated.")
  gameState.defeatedMonsters.keys.map(monsterType => println(s"${monsterType}'s killed: ${gameState.defeatedMonsters.get(monsterType).get}"))
  println(s"Dungeon level reached: ${gameState.dungeonLevel}")
  println("======== Ending gear ========")
  println(s"Weapon: ${player.weapon.name} (${player.weapon.damage._1}-${player.weapon.damage._2}, ${player.weapon.attackBonus})")
  println(s"Armor: ${player.armor.name} (${player.armor.armorBonus})")
  player.inventory.items.foreach(item => {
    print(s"${item._2.head.name}")
    if (item._2.length > 1) {
      print(s" x${item._2.length}")
    }
    println("")
  })
  println("**********************************")
}

class GameState(player:Player, screen: Scurses) {
  def getFloorItems = floors(dungeonLevel - 1).droppedItems
  def getMonsters = floors(dungeonLevel - 1).monsters
  def getTraps = floors(dungeonLevel - 1).traps
  def getCurrentFloor = floors(dungeonLevel - 1)

  val renderer = new Renderer(this, screen)
  var defeatedMonsters = Map[String,Int]()
  var tiles = mutable.Seq[mutable.Seq[Tile]]()
  var rooms: mutable.Seq[Room] = mutable.Seq[Room]()
  var floors: Seq[Floor] = Seq[Floor]()
  var shouldGenerateMonster = true
  var monsters: List[Monster] = List[Monster]()
  var droppedItems = List[Item]()
  var traps = Seq[Trap]()

  var currTileDescription: String = "Nothing is here."
  var roundMessage: String = ""
  var monsterActionMessages = Map[String, Int]()
  var shrine: Shrine = new HealthShrine(new Position(-1, -1)) // creat a fake shrine for now
  var dungeonLevel = 0
  var monstersSlain = 0
  var debugMode = false

  def resetState = {
    tiles = mutable.Seq[mutable.Seq[Tile]]()
    rooms = mutable.Seq[Room]()
    monsters = List[Monster]()
    droppedItems = List[Item]()
  }

  def createFloor = {
    resetState
    for (x <- 0 to NUM_COLS - 1) {
      tiles = tiles :+ Seq[Tile]()
      for (y <- 0 to NUM_ROWS - 1) {
        val thisPos = new Position(x, y)
        tiles(x) = tiles(x) :+ new EmptyTile(thisPos)
      }
    }

    // setup map
    val newFloor = Floor(dungeonLevel, bossLevel = dungeonLevel == 5)
    floors = floors :+ newFloor
    randomMap
    newFloor.rooms = rooms
    newFloor.populate
    dungeonLevel = dungeonLevel + 1
  }

  createFloor

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


    shrine = new HealthShrine(new Position(-1, -1)) // create a fake shrine for now
    val shouldCreateShrine = Random.nextInt(100) + 1
    if (shouldCreateShrine < 20) {
      shrine = generateShrine
    }

    // Populate Neighbors of tiles
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
    setSurroundingTilesVisible(getPlayer.position)
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
            if (getTileAtPosition(x, y).get.isInstanceOf[EmptyTile] && (x == newRoom.startPosition.x || x == newRoom.startPosition.x + newRoom.width)) {
              tiles(x)(y) = new VerticalWall(new Position(x, y))
            } else if (getTileAtPosition(x, y).get.isInstanceOf[EmptyTile] && (y == newRoom.startPosition.y || y == newRoom.startPosition.y + newRoom.height)) {
              tiles(x)(y) = new HorizontalWall(new Position(x, y))
            }
            else {
              tiles(x)(y) = new FloorTile(new Position(x, y))
            }
          }
        }

        if (listOfRooms.length == 0) {
          player.position = newRoom.getCenter
        }
        else {
          // Tunnel out a hallway to the previous room
          val prevCenter = listOfRooms(listOfRooms.length - 1).getRandomValidPosition

          val newRoomEntrance = newRoom.getRandomValidPosition
          val x1 = newRoomEntrance.x
          val y1 = newRoomEntrance.y
          val x2 = prevCenter.x
          val y2 = prevCenter.y


          Random.nextInt(1) match {
            case 0 => {
              for (x <- Math.min(x1, x2) to Math.max(x1, x2)) {
                tiles(x)(y1).isInstanceOf[FloorTile] match {
                  case true => ()
                  case false => {
                    tiles(x)(y1) = new FloorTile(new Position(x, y1), "#")
                  }
                }
              }

              for (y <- Math.min(y1, y2) to Math.max(y1, y2)) {
                tiles(x2)(y).isInstanceOf[FloorTile] match {
                  case true => ()
                  case false => tiles(x2)(y) = new FloorTile(new Position(x2, y), "#")
                }
              }
            }
            case 1 => {
              for (y <- Math.min(y1, y2) to Math.max(y1, y2)) {
                tiles(x1)(y).isInstanceOf[FloorTile] match {
                  case true => ()
                  case false => tiles(x1)(y) = new FloorTile(new Position(x1, y), "#")
                }
              }

              for (x <- Math.min(x1, x2) to Math.max(x1, x2)) {
                tiles(x)(y2).isInstanceOf[FloorTile] match {
                  case true => ()
                  case false => tiles(x)(y2) = new FloorTile(new Position(x, y2), "#")
                }
              }
            }
          }
        }

        // Add room to the list
        listOfRooms = listOfRooms :+ newRoom
      }
    }


    if (dungeonLevel - 1 == 5) {
      Scurses { screen =>
        // The screen will only be in Scurses mode inside this block
        // Scurses will reset the terminal buffer and everything when outside

        // Get the current terminal size
        val (w, h) = screen.size

        val prompt =
          """
            |You descend into the depths of the Tomb. The stench of the Necromancer assaults
            |your nostrils. In the distance, you hear a sinister laugh...
            |(press any key to continue)
            |""".stripMargin
        // Put some strings in the middle of the screen
        screen.put(w/2 - prompt.length/2, h/2 + 1, prompt, Colors.BRIGHT_BLACK)
        // Flush the buffer
        screen.refresh()
        // Wait for an input without storing it
        screen.keypress()
      }
      // Final level only has Cem Hial
      monsters = List[Monster](new CemHial(listOfRooms.last.getRandomValidPosition))
      floors(dungeonLevel).monsters = monsters
    }
    else {
      // create stairs
     floors(dungeonLevel).droppedItems = floors(dungeonLevel).droppedItems :+ new Item(listOfRooms.last.getRandomValidPosition, "v", "The stairwell descends into darkness", "DOWN_STAIR", displayColor = Colors.DIM_MAGENTA)
    }
    listOfRooms
  }

  def generateShrine(): Shrine = {
    val randPos = if (rooms.length > 1) {
      rooms(Random.nextInt(rooms.length - 1)).getRandomValidPosition
    } else if (rooms.length > 0) {
      rooms(0).getRandomValidPosition
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

  def performPlayerAttack(monster:Monster) = {
    playerHasValidTarget(player, monster) match {
      case Some(m) => {
        val attack = player.performAttack(m.armorClass)
        if (attack >= m.armorClass) {
          val damage = player.weapon.attack(Some(m), Some(player))
          player.appendActionMessage(s"You stab with your ${player.weapon.name} dealing ${damage} damage!")
          m.health = m.health - damage
        }
        if (m.health <= 0) {
          monsterActionMessages = monsterActionMessages + (s"${m.name} was slain!\n" -> Colors.DIM_WHITE)
          m.dropLoot match {
            case Some(loot) => {
              floors(dungeonLevel - 1).droppedItems = floors(dungeonLevel - 1).droppedItems :+ loot
            }
            case None => None

          }
          monstersSlain += 1
          floors(dungeonLevel - 1).getMonsters.filter(monster => monster != m)
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
      }
      case None => {
        val currPlayerPosition = player.position
        player.position = getTileAtPosition(newPos.x, newPos.y) match {
          case Some(tile) => {
            if (tile.passable || player.canAvoidObstacles) {
              getTileAtPosition(currPlayerPosition.x, currPlayerPosition.y).get.occupied = false
              getTileAtPosition(newPos.x, newPos.y).get.occupied = true

              setTilesNotVisible
              setSurroundingTilesVisible(newPos)

              newPos
            } else {
              player.position
            }
          }
          case None => newPos
        }
      }
    }
    return true
  }

  def tick(action: Int): Boolean = {
    print("\033c")

    var playerPerformedAction = false
    val playerIsAlive = true

    if (debugMode) {
      monsterActionMessages = monsterActionMessages + (s"${dungeonLevel}: ${player.position}" -> Colors.DIM_YELLOW)
      monsterActionMessages = monsterActionMessages + (s"${floors(dungeonLevel - 1).monsters.map(m => m.name.concat(m.position.toString))}" -> Colors.DIM_RED)
    }

    playerPerformedAction = action match {
      case QUAFF_POTION => {
        renderer.renderInventoryScreen()
        screen.put(0, 0, ("(C)onsume what? "))
        screen.refresh()


        val input = try {
          scala.io.StdIn.readInt
        }
        catch {
          case _: Throwable => 0
        }
        var tookAction = false
        if (input > 0 && input <= player.inventory.items.size) {
          val itemToConsume = player.inventory.getItem(input - 1) // display is one-based

          if (itemToConsume.isInstanceOf[Potion]) {
            player.consumeConsumable(itemToConsume.asInstanceOf[Potion])
            tookAction = true
          }
          else if (itemToConsume.isInstanceOf[Scroll]) {
            player.consumeConsumable(itemToConsume.asInstanceOf[Scroll])
            tookAction = true
//            itemToConsume match {
//              case scroll:IdentifyScroll => {
//                player.consumeConsumable(scroll)
//                tookAction = true
//              }
//            }
          }
          else {
            player.appendActionMessage("That item can't be consumed!")
          }
        }

        tookAction
      }
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
        floors(dungeonLevel - 1).droppedItems.filter(item => item.position.x == player.position.x && item.position.y == player.position.y).headOption match {
          case Some(item) => {
            if (item.id == "DOWN_STAIR") {
              createFloor
            }
            else if(item.id == "NIGHT_BLADE") {
              renderer.renderPlayerActions
              renderer.renderMonsterActions(monsterActionMessages)
              return false
            }
            else {
              item.interact(player)
              floors(dungeonLevel - 1).droppedItems = floors(dungeonLevel - 1).droppedItems.filterNot( i => i == item)
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
        true
      }
      case EQUIP => {
        renderer.renderInventoryScreen()
        screen.put(0, 0, ("(E)quip what? "))
        screen.refresh()


        val input = try {
          scala.io.StdIn.readInt
        }
        catch {
          case _: Throwable => 0
        }
        if (input > 0 && input <= player.inventory.items.size) {
          val itemToEquip = player.inventory.getItem(input - 1) // display is one-based
          if (itemToEquip.isInstanceOf[Weapon]) {
            player.weapon = itemToEquip.asInstanceOf[Weapon]
            player.appendActionMessage(s"You are now wielding a ${itemToEquip.name}")
          }
          else if (itemToEquip.isInstanceOf[Armor]) {
            player.armor = itemToEquip.asInstanceOf[Armor]
            player.appendActionMessage(s"You are now wearing a ${itemToEquip.name}")
          }
          else {
            player.appendActionMessage("That item can't be equipped!")
          }
        }
        false
      }
      case UNEQUIP => {

        false
      }
      case DISPLAY_HELP => {
        renderer.sideContent = if (renderer.sideContent == "HELP_MENU") "EMPTY" else "HELP_MENU"
        false
      }
      case TOGGLE_INVENTORY => {
        renderer.sideContent = if (renderer.sideContent == "INVENTORY") "EMPTY" else "INVENTORY"
        false
      }
      case RUN_COMMAND => {
        runCommand
        false
      }
      case OBSERVE_COMMAND => {
        screen.hideCursor()
        val cursor = Cursor(new Position(player.position.x, player.position.y))
        cursor.enabled = true
        renderer.render(false)
        renderer.renderStatsBar(Some(cursor.position))
        cursor.render(screen)
        screen.refresh

        val setToRaw = Array[String]("/bin/sh", "-c", "stty raw </dev/tty")
        val setToCooked = Array[String]("/bin/sh", "-c", "stty cooked </dev/tty")



        Runtime.getRuntime.exec(setToRaw).waitFor()

        var input = Console.in.read

        Runtime.getRuntime.exec(setToCooked).waitFor()
        while (input != OBSERVE_COMMAND && input != EXIT) {
          input match {
            case MOVE_LEFT => cursor.pos.x = if (moveIfVisible(cursor.pos.x - 1, cursor.pos.y)) DungeonHelper.clamp(cursor.pos.x - 1, 0, NUM_COLS) else cursor.pos.x
            case MOVE_RIGHT => cursor.pos.x = if (moveIfVisible(cursor.pos.x + 1, cursor.pos.y)) DungeonHelper.clamp(cursor.pos.x + 1, 0, NUM_COLS) else cursor.pos.x
            case MOVE_UP => cursor.pos.y = if (moveIfVisible(cursor.pos.x, cursor.pos.y - 1)) DungeonHelper.clamp(cursor.pos.y - 1, 0, NUM_ROWS) else cursor.pos.y
            case MOVE_DOWN => cursor.pos.y = if (moveIfVisible(cursor.pos.x, cursor.pos.y + 1)) DungeonHelper.clamp(cursor.pos.y + 1, 0, NUM_ROWS) else cursor.pos.y
            case MOVE_UP_LEFT => {
              val x = cursor.pos.x - 1
              val y = cursor.pos.y - 1
              if (moveIfVisible(x, y)) {
                cursor.pos.x = x
                cursor.pos.y = y
              }
            }
            case MOVE_UP_RIGHT => {
              val x = cursor.pos.x + 1
              val y = cursor.pos.y - 1
              if (moveIfVisible(x, y)) {
                cursor.pos.x = x
                cursor.pos.y = y
              }
            }
            case MOVE_DOWN_LEFT => {
              val x = cursor.pos.x - 1
              val y = cursor.pos.y + 1
              if (moveIfVisible(x, y)) {
                cursor.pos.x = x
                cursor.pos.y = y
              }
            }
            case MOVE_DOWN_RIGHT => {
              val x = cursor.pos.x + 1
              val y = cursor.pos.y + 1
              if (moveIfVisible(x, y)) {
                cursor.pos.x = x
                cursor.pos.y = y
              }
            }
            case _ =>
          }
          renderer.render(false)
          renderer.renderStatsBar(Some(cursor.position))
          cursor.render(screen)
          //screen.put(0, NUM_ROWS + 2 + 4, s"${get}", descriptionTextColor)

          screen.refresh
          Runtime.getRuntime.exec(setToRaw).waitFor()

          input = Console.in.read
          Runtime.getRuntime.exec(setToCooked).waitFor()

        }

        //screen.hideCursor()
        cursor.enabled = false
        false
      }
      case ESCAPE | EXIT => {
        // quit the game
        return false
      }
      case _ => false
    }

    if (playerPerformedAction) {
      // Apply conditions
      player.conditions.foreach(condition => {
        condition.apply
      })

      // check for trap activation
      val trap = getTraps.find(trap => trap.pos.x == player.position.x && trap.pos.y == player.position.y)
      if (trap.isDefined) {
        trap.get.identified = true
        trap.get.trigger(player)
      }
      // Perform monster actions
      floors(dungeonLevel - 1).getMonsters.map(monster => {
        if (monster.isAlive) {
          if (monster.conditions.length > 0) {
            monster.conditions.foreach(condition => {
              condition.name match {
                case "Poisoned" => monsterActionMessages = monsterActionMessages + (s"${monster.name} was hurt by poison!" -> Colors.DIM_WHITE)
                case "Burning" => monsterActionMessages = monsterActionMessages + (s"${monster.name} was hurt by the flames!" -> Colors.DIM_WHITE)
              }
              condition.apply
            })
          }

          if (monster.isAlive) {
            if (monsterHasValidTarget(monster, player) && monster.isAlive) {
              val hitRoll = monster.performAttack
              if (hitRoll >= player.armorClass + player.armor.armorBonus) {
                val damage = monster.calculateDamage
                monsterActionMessages = monsterActionMessages + (s"${monster.name} ${monster.weapon.getAttackText} dealing ${damage} damage." -> Colors.DIM_RED)
                player.health = player.health - damage
              }
              else {
                monsterActionMessages = monsterActionMessages + (s"The ${monster.name} missed you." -> Colors.DIM_WHITE)
              }
            }
            val playerTile = getTileAtPosition(player.position.x, player.position.y)
            val newPos = monster.move(playerTile, tiles, getTileAtPosition(monster.position.x, monster.position.y))
            getTileAtPosition(newPos.x, newPos.y) match {
              case Some(tile) => {
                if ((tile.passable || monster.canAvoidObstacles) && !tile.occupied) {
                  getTileAtPosition(monster.position.x, monster.position.y).get.occupied = false
                  monster.position = newPos
                  getTileAtPosition(monster.position.x, monster.position.y).get.occupied = true
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
    }


    renderer.render()

    player.endRound
    monsterActionMessages = Map[String, Int]()

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
    floors(dungeonLevel - 1).getMonsters.filter(monster => monster.isAlive && monster.position.x == position.x && monster.position.y == position.y).headOption
  }

  def getTileAtPosition(x: Int, y: Int): Option[Tile] = {
    if (x < 0 || x >= NUM_COLS || y < 0 || y >= NUM_ROWS) return None
    Option(tiles(x)(y))
  }

  def setSurroundingTilesVisible(playerPos: Position) = {
    //// update tiles with visibility
    for (x <- player.position.x - player.sightDistance to player.position.x + player.sightDistance) {
      for (y <- player.position.y - player.sightDistance to player.position.y + player.sightDistance) {
        getTileAtPosition(x, y) match {
          case Some(tile) => {
            tile.currentlyVisible = true
            tile.hasBeenSeen = true
          }
          case None => ()
        }
      }
    }
  }

  def setTilesNotVisible = {
    tiles.foreach(row => {
      row.foreach(tile => tile.currentlyVisible = false)
    })
  }

  private def moveIfVisible(x: Int, y: Int): Boolean = {
    val tile = getTileAtPosition(x, y)
    tile.isDefined && (tile.get.currentlyVisible || tile.get.hasBeenSeen)
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
            case "axe" => player.weapon = new FineWeaponDecorator(new GreatAxe)
            case "night_blade" => player.weapon = new NightBlade()
            case "dagger" => player.weapon = new FineWeaponDecorator(new Dagger)
            case "sword" => player.weapon = new FineWeaponDecorator(new ShortSword)
            case "spear" => player.weapon = new Spear()
            case "dragon_scale" => player.armor = new DragonScale()
          }
        }
      }
      case "heal" => {
        player.health = player.maxHealth
      }
      case "reveal" => {
        player.sightDistance = 1000
        setSurroundingTilesVisible(player.position)
      }
      case "godmode" => {
        player.health = 99999
        player.attackBonus = 999
        player.canAvoidObstacles = true
        player.sightDistance = 1000
      }
      case "debug" => {
        debugMode = !debugMode
      }
      case _ => println("Unknown command")
    }
  }
}

case class Cursor(pos: Position) {
  var enabled: Boolean = false
  val position = pos
  def render(s: Scurses) = {
    if (enabled) {
      s.put(position.x, position.y, "X", Colors.BRIGHT_WHITE)
    }
  }
}
