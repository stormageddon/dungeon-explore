package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Monster.Goblin
import com.scalaplayground.dungeonexplore.Position.Position
import org.mockito.MockitoSugar
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
//import org.scalatest.matchers._
//import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfterEach}

class TrapTest extends AnyFlatSpec
  with Matchers
  with MockitoSugar
  with BeforeAndAfterEach{

  var testTrap: Trap = _

  override def beforeEach() = {
    implicit def mockRandImplicit(n: Int): Int = 2
    testTrap = DartTrap(new Position(0, 0))(mockRandImplicit)
  }

  behavior of "Trap"

  it should "not be identified by default" in {
    testTrap.identified mustBe false
    testTrap.displayChar mustBe "."
  }

  it should "hide the trap as a floor when not identified" in {
    testTrap.identified = true

    testTrap.displayChar mustBe "^"

    testTrap.identified = false
    testTrap.displayChar mustBe "."
  }

  behavior of "Dart Trap"

  it should "damage a target by a random amount" in {

    val target = new Goblin(new Position(1,1))
    target.health = 10

    testTrap.trigger(target)

    target.health mustBe 7
  }

  it should "deal a minimum of 1 damage" in {
    implicit def mockRandImplicit(n: Int): Int = 0
    testTrap = DartTrap(new Position(0, 0))(mockRandImplicit)
    val target = new Goblin(new Position(1,1))
    target.health = 10

    testTrap.trigger(target)

    target.health mustBe 9
  }
}

