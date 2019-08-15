package com.scalaplayground.dungeonexplore.Game

import net.team2xh.scurses.{Colors, Scurses}

import scala.util.Random
import com.scalaplayground.dungeonexplore.Monster._
import com.scalaplayground.dungeonexplore.Constants._
import com.scalaplayground.dungeonexplore.Player



object Game extends App {
  def createPlayer: Player = {
    println("What is your name, traveler?")
    new Player(scala.io.StdIn.readLine(">> "))
  }

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
      println(s"${player.name} was slain by a ${gameState.monster.name}. RIP.")
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
      var input = scala.io.StdIn.readLine(">> ")
      println(input)

      if (input == "q") {
        isPlaying = false
      }
      else {
        isPlaying = gameState.tick(input)
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
  def generateMonster(): Monster = {
    if (monstersSlain == 10) {
      println("The door before you creaks open and an inhuman howl escapes from inside. A grayish light reveals the final resting place of Cem Hial...\n\n\n")
      new CemHial()
    }
    else {
////      Random.nextInt(100) match {
////        case it if 0 until 25 contains it => new Goblin()
////        case it if 25 until 50 contains it => new Kobold()
////        case it if  50 until 75 contains it => new GiantRat()
////        case it if 75 until 90 contains it => new Orc()
////        case it if 90 until 100 contains it => new Wolf()
//      }
      new GiantRat()
    }
  }

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
            player.numPotions = player.numPotions + 1
            println(s"${monster.name} dropped a potion! You now have ${player.numPotions}.")
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
            case _ => ""
          }
        }
      }
    }
    else {
      println(s"You missed the ${monster.name}")
    }

    return true
  }

  var defeatedMonsters = Map[String,Int]()

  var monster = generateMonster()
  var monstersSlain = 0

  println(s"You are jumped by a ${monster.name}!")

  def tick(action:String): Boolean = {
    print("\033c")
    val colNum = NUM_COLS
    val rowNum = NUM_ROWS

    for (y <- 0 to rowNum - 1) {
      for (x <- 0 to colNum - 1) {
        if (player.position.x == x && player.position.y == y) {
          print("p")
        }
        else if (monster.position.x == x && monster.position.y == y) {
          print(monster.displayChar)
        }
        else {
          print(".")
        }
        print(" ")
      }
      println("")
    }

    println(s"MATCH: ${action}")

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
      case _ => println("You did something")
    }

    if (monster == null) {
      return playerIsAlive
    }

    // Perform monster action
    if (monster.isAlive) {
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
      monster.position = monster.move
    }
    else {
      // Kick open a door
      print("You kick open another door.")
      Random.nextInt(100) match {
        case it if 0 until 25 contains it => {
          Random.nextInt(100) match {
            case it if 0 until 50 contains it => {
              println("You discover a strange shrine.\nYou feel better as you look at it.")
              player.health = STARTING_PLAYER_HEALTH
            }
            case it if 50 until 75 contains it => {
              println("You discover a strange shrine.\nIt makes you feel stronger to look at.")
              player.attackBonus = player.attackBonus + 2
            }
            case it if 75 until 90 contains it => {
              println("You discover a strange shrine.\nYou dip your weapon in and it gleams brilliantly.")
              player.weapon.attackBonus = player.weapon.attackBonus + 2
              player.weapon.name = s"Blessed ${player.weapon.name}"
            }
            case it if 90 until 100 contains it => {
              println("You discover a strange shrine.\nYou feel sick looking at it.")
              player.health = player.health - 1
            }
          }
          monster = null
        }
        case _ => {
          monster = generateMonster()
          println(s"A ${monster.name} attacks!")
        }
      }
    }

    if (playerDidMove) {
      playerHasValidTarget(player, monster) match {
        case Some(m) => {
          println("ATTACKING!!!!!")
          m.health = m.health - player.performAttack
        }
        case None => println("No target to attack!")
      }
      player.performAttack
    }

    return playerIsAlive
  }

  def playerHasValidTarget(player: Player, monster: Monster): Option[Monster] = {
    if ((math.abs(player.position.x - this.monster.position.x) <= 1) && (math.abs(player.position.y - monster.position.y) <= 1)) {
      return Some(monster)
    }
    None
  }
}
