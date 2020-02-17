package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Monster.{CharacterObject, Monster}

import scala.util.Random

abstract class Condition {
  val name: String

  def apply
}

case class Poisoned(val target: CharacterObject) extends Condition {
  override val name: String = "Poisoned"
  val effectRounds: Int = Random.nextInt(4) + 1
  var currRound: Int = 0
  var damage = 1

  def apply: Unit = {
    if (target.isInstanceOf[Monster]) {

    }
    target.health = target.health - 1
    currRound = currRound + damage

    if (currRound >= effectRounds) {
      target.conditions = target.conditions.filterNot(c => c == this)
    }
  }
}
