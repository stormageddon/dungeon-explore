package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Monster.{CharacterObject, Monster}
import com.scalaplayground.dungeonexplore.constants.Constants.DEFAULT_SIGHT_DISTANCE

import scala.util.Random

sealed trait Condition {
  val name: String

  def apply
}

case class Poisoned(val target: CharacterObject) extends Condition {
  override val name: String = "Poisoned"
  val effectRounds: Int = Random.nextInt(4) + 1
  var currRound: Int = 0
  var damage = 1

  def apply: Unit = {
    target.health = target.health - damage
    currRound = currRound + 1

    if (currRound > effectRounds) {
      target.conditions = target.conditions.filterNot(c => c == this)
    }
  }
}

case class Burning(val target: CharacterObject) extends Condition {
  override val name: String = "Burning"
  val effectRounds: Int = Random.nextInt(4) + 1
  var currRound: Int = 0
  var damage: Int = Random.nextInt(4) + 1

  def apply: Unit = {
    target.health = target.health - damage
    currRound = currRound + 1

    if (currRound > effectRounds) {
      target.conditions = target.conditions.filterNot(c => c == this)
    }
  }
}

case class HardenedArmor(val target: CharacterObject) extends Condition {
  override val name: String = "Hardened armor"
  val effectRounds: Int = Random.nextInt(6) + 1
  var currRound: Int = 0
  target.armorClass = target.armorClass + 2

  def apply: Unit = {

    currRound = currRound + 1
    if (currRound > effectRounds) {
      target.conditions = target.conditions.filterNot(c => c == this)
      target.armorClass = target.armorClass - 2
    }
  }
}

case class Telepathic(val target: CharacterObject) extends Condition {
  override val name: String = "Telepathic"
  val effectRounds: Int = Random.nextInt(12) + 3
  var currRound: Int = 0
  target match {
    case t:Player => t.sightDistance = 1000
    case _ =>
  }

  def apply: Unit = {
    currRound = currRound + 1
    if (currRound > effectRounds) {
      target.conditions = target.conditions.filterNot(c => c == this)
      target match {
        case t:Player => t.sightDistance = DEFAULT_SIGHT_DISTANCE
        case _ =>
      }
    }
  }
}
