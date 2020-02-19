package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Monster.CharacterObject
import com.scalaplayground.dungeonexplore.Position.Position

import scala.util.Random

abstract class Trap(val pos: Position) {
  def displayChar: String = (() => return if (this.identified) "^" else ".")()
  var identified: Boolean
  var description: String = ""
  def trigger(target: CharacterObject): Unit
}

case class DartTrap(override val pos: Position)(implicit getRandomInt: (Int) => Int) extends Trap(pos) {
  override var identified = false
  description = "Dart Trap"

  def trigger(target: CharacterObject): Unit = {
    if (target.isInstanceOf[Player]) {
      target.asInstanceOf[Player].appendActionMessage("You set off a trap! The dart stabs you.")
    }
    val damage = getRandomInt(3)
    target.health = target.health - (damage + 1)
  }
}

case class AlarmTrap(override val pos: Position) extends Trap(pos) {
  override var identified = false
  description = "Alarm trap"

  def trigger(target: CharacterObject): Unit = {

  }
}

object implicits {
  implicit def getRandomInt(n: Int = 10): Int = {
    Random.nextInt(n)
  }

}
