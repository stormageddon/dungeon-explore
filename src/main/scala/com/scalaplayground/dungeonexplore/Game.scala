package com.scalaplayground.dungeonexplore.Game

import net.team2xh.scurses.{Colors, Scurses}

import scala.util.Random
import com.scalaplayground.dungeonexplore.Monster._
import com.scalaplayground.dungeonexplore.Constants._
import com.scalaplayground.dungeonexplore.Item.Item
import com.scalaplayground.dungeonexplore.Position.Position
import com.scalaplayground.dungeonexplore.Shrine._
import com.scalaplayground.dungeonexplore.{DungeonHelper, Player}

import scala.collection.parallel.ThreadPoolTasks



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


  val player = createPlayer
  println(s"Hello ${player.name}. You currently have ${player.health} hp")

  val gameState = new GameState(player)
  var isPlaying = true

  print("\033c")
  val colNum = NUM_COLS
  val rowNum = NUM_ROWS

  for (y <- 0 to rowNum - 1) {
    for (x <- 0 to colNum - 1) {
      if (player.position.x == x && player.position.y == y) {
        print("p")
      }
      else {
        print(".")
      }
      print(" ")
    }
    println("")
  }

  while(isPlaying) {

    if (player.health <= 0) {
      println("**********************************")
      println(s"${player.name} was slain. RIP.")
      isPlaying = false
    }
    else {
//      println("\n\n\n=======")
//      println(s"${player.name} (${player.health})")
//      println(
//        s"""
//           |Select an action:
//           |1. Attack
//           |2. Quaff a potion
//           |3. Enter another room
//           |4. Quit
//         """.stripMargin)


      //val input = scala.io.StdIn.readChar()

      val input = s.keypress.toChar


      println(input)
      val key = input.toString.asInstanceOf[String]
      print("\033c")

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
  println(s"Weapon: ${player.weapon.name} (${player.weapon.damage._1}d${player.weapon.damage._2}, ${player.weapon.attackBonus})")
  println(s"Armor: ${player.armor.name} (${player.armor.armorBonus})")
  println("**********************************")
}



class GameState(player:Player) {
  val dungeonHelper = new DungeonHelper
  var defeatedMonsters = Map[String,Int]()

  var monsters: List[Monster] = List[Monster](generateMonster().get)
  var monstersSlain = 0
  var shrine = generateShrine()
  var droppedItems = List[Item]()


  def generateShrine(): Shrine = {
      Random.nextInt(100) match {
        case it if 0 until 50 contains it => return new HealthShrine(new Position(Random.nextInt(20),Random.nextInt(20)))
        case it if 50 until 75 contains it => return new StrengthShrine(new Position(Random.nextInt(20),Random.nextInt(20)))
        case it if 75 until 90 contains it => return new HolyShrine(new Position(Random.nextInt(20),Random.nextInt(20)))
        case it if 90 until 100 contains it => return new CursedShrine(new Position(Random.nextInt(20),Random.nextInt(20)))
      }
  }

  def generateMonster(): Option[Monster] = {
    if (monsters != null && monsters.filter(m => m.isAlive).length >= MAX_MONSTERS_ALIVE) {
      return None
    }
    if (monstersSlain == 10) {
      println("The door before you creaks open and an inhuman howl escapes from inside. A grayish light reveals the final resting place of Cem Hial...\n\n\n")
      return Some(new CemHial())
    }
    else {
      return Random.nextInt(100) match {
        case it if 0 until 25 contains it => Some(new Goblin())
        case it if 25 until 50 contains it => Some(new Kobold())
        case it if  50 until 75 contains it => Some(new GiantRat())
        case it if 75 until 90 contains it => Some(new Orc())
        case it if 90 until 100 contains it => Some(new Wolf())
      }
      //return Some(new Orc(new Position(Random.nextInt(NUM_ROWS), Random.nextInt(NUM_COLS))))
    }
  }

  /*
  def openDoor: Unit = {
    println("You kick open a new door")
    Random.nextInt(5) match {
      case 0 => {
        val trapDamage = Random.nextInt(3) + 1
        println(s"The door was trapped! You take ${trapDamage} points of damage!")
      }
      case _ => ()
    }
    monster = generateMonster()
    println(s"You are jumped by a ${monster.name}!")
  }
  */

  /*
  def performPlayerAction: Boolean = {
    if (monster == null) {
      println("This room is empty!")
      return true
    }
    val attackRoll = player.performAttack
    println(s"attack roll of ${attackRoll} vs AC ${monster.armorClass + monster.armor.armorBonus}")

    if (attackRoll >= monster.armorClass + monster.armor.armorBonus) {
      val damage = player.calculateDamage
      monster.health = monster.health - damage
      if (monster.health <= 0) {
        println(s"You defeated the ${monster.name}!")
        if (defeatedMonsters.contains(monster.name)) {
          defeatedMonsters = defeatedMonsters ++ Map[String,Int](monster.name -> (defeatedMonsters(monster.name) + 1))
        }
        else {
          defeatedMonsters = defeatedMonsters ++ Map[String,Int](monster.name -> 1)
        }
        monstersSlain = monstersSlain + 1
        if (monster.name == "Cem Hial, the Necromancer") {
          println("With one final wail the Necromancer disappears into nothing. Congratulations! The town is saved!")
          return false
        }
        val loot = monster.dropLoot match {
          case Some("potion") => {
            print("dropping potion")
            //player.numPotions = player.numPotions + 1
            //println(s"${monster.name} dropped a potion! You now have ${player.numPotions}.")
            droppedItems = droppedItems :+ new Item(new Position(Random.nextInt(NUM_ROWS), Random.nextInt(NUM_COLS)), "!")
          }
          case _ => None
        }
        println(loot)

        if(monster.weapon.isDroppable) {
          println(
            s"""
               |${monster.name} dropped their ${monster.weapon.name}. Pick it up?
               |1. Yes
               |2. No
                 """.stripMargin
          )
          scala.io.StdIn.readLine(">> ").toInt match {
            case 1 => {
              player.weapon = monster.weapon
              println(s"You wield your new ${monster.weapon.name} threateningly.")
            }
            case _ => ""
          }
        }
        if (monster.armor.isDroppable) {
          println(
            s"""
               |${monster.name} dropped their ${monster.armor.name}. Pick it up (${monster.armor.armorBonus - player.armor.armorBonus})?
               |1. Yes
               |2. No
                 """.stripMargin
          )
          scala.io.StdIn.readLine(">> ").toInt match {
            case 1 => {
              player.armor = monster.armor
              println(s"You don your new ${monster.armor.name} armor. It feels a bit snug.")
            }
            case _ => Unit
          }
        }
      }
    }
    else {
      println(s"You missed the ${monster.name}")
    }

    return true
  }
  */

  def tick(action: String): Boolean = {
    //print("\033c")
    val colNum = NUM_COLS
    val rowNum = NUM_ROWS

    for (y <- 0 to rowNum - 1) {
      for (x <- 0 to colNum - 1) {
        if (player.position.x == x && player.position.y == y) {
          // Check for shrine usage
          if (shrine != null && shrine.position.x == player.position.x && shrine.position.y == player.position.y) {
            shrine.interact(player)
            shrine = generateShrine()
          }
          // Check for loot
//          droppedItems.filter(item => item.position.x == player.position.x && item.position.y == player.position.y).headOption match {
//            case Some(item) => {
//              item.interact(player)
//              droppedItems = droppedItems.filterNot( i => i == item)
//            }
//            case None => Unit
//          }
          print("p")
        }
        else if (monsters.filter(m => m.position.x == x && m.position.y == y && m.isAlive).length > 0) {
          monsters.filter(m => m.position.x == x && m.position.y == y && m.isAlive).headOption match {
            case Some(monster) => print(monster.displayChar)
            case None => Unit
          }

        }
        else if (shrine != null && shrine.position.x == x && shrine.position.y == y) {
          print(shrine.displayChar)
        }
        else if ( droppedItems.filter(i => i.position.x == x && i.position.y == y).length > 0 ) {
          droppedItems.filter(i => i.position.x == x && i.position.y == y).headOption match {
            case Some(item) => item.render
            case None => Unit
          }
        }
        else {
          print(".")
        }
        print(" ")
      }
      println("")
    }


    renderStatsBar()

    var playerDidMove = false
    val playerIsAlive = true
    action match {
      //case "" => playerIsAlive = performPlayerAction
      case "q" => player.quaffPotion
      //case "o" => openDoor
      case "w" => {
        player.position = player.move(0,-1)
        playerDidMove = true
      }
      case "a" => {
        player.position = player.move(-1, 0)
        playerDidMove = true
      }
      case "s" => {
        player.position = player.move(0, 1)
        playerDidMove = true
      }
      case "d" => {
        player.position = player.move(1, 0)
        playerDidMove = true
      }
      case "u" => {
        droppedItems.filter(item => item.position.x == player.position.x && item.position.y == player.position.y).headOption match {
          case Some(item) => {
            item.interact(player)
            droppedItems = droppedItems.filterNot( i => i == item)
          }
          case None => print("There is nothing to use here.")
        }
      }
      case _ => Unit
    }

    if (monsters.length == 0) {
      return playerIsAlive
    }

    // Perform monster actions
    monsters.map(monster => {
      if (monster.isAlive) {
        playerHasValidTarget(player, monster) match {
          case Some(m) => {
            print("ATTACKING!!!!!")
            val attack = player.performAttack
            if (attack >= m.armorClass) {
              m.health = m.health - player.weapon.attack
            }
            if (m.health <= 0) {
              m.dropLoot match {
                case Some("potion") => {
                  droppedItems = droppedItems :+ new Item(new Position(m.position.x, m.position.y), "!")
                }
                case _ => None
              }

              monsters = monsters ++ List[Monster](generateMonster.get)
            }
          }
          case None => Unit
        }

        if (monsterHasValidTarget(monster, player)) {
          val hitRoll = monster.performAttack
          println(s"attack roll of ${hitRoll} vs AC ${player.armorClass + player.armor.armorBonus}")
          if (hitRoll >= player.armorClass + player.armor.armorBonus) {
            val damage = monster.calculateDamage
            println(s"${monster.name} swings at you with their ${monster.weapon.name} dealing ${damage} damage")
            player.health = player.health - damage
          }
          else {
            println(s"The ${monster.name} missed you.")
          }
        }

        monster.position = monster.move(Some(player.position))
      }
//      else {
//        // Kick open a door
//        print("You kick open another door.")
//        monster = generateMonster()
//      }
    })


  //  if (playerDidMove) {

      //player.performAttack
    //}

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

  def renderStatsBar(): Unit = {
    val p = getPlayer
    println(s"${p.name}")
    println(s"HP: ${p.health}    AC: ${p.armorClass}     WIELDING: ${p.weapon.name}     POTIONS (q): ${p.numPotions}")
    //println(s"Nothing is here")
    println(s"Player pos: ${player.position}")
    println("Monsters:")
    monsters.map(monster => {
      if (monster.isAlive) {
        println(s"${monster.name} is at ${monster.position}")
      }
    })
  }


}
