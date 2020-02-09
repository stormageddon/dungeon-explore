package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Monster.Goblin
import com.scalaplayground.dungeonexplore.Position.Position
import org.scalatest.mockito.MockitoSugar
import org.scalatest.{BeforeAndAfterEach, FlatSpec, Matchers}

class TrapTest extends FlatSpec
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
    testTrap.identified shouldBe false
    testTrap.displayChar shouldBe "."
  }

  it should "hide the trap as a floor when not identified" in {
    testTrap.identified = true

    testTrap.displayChar shouldBe "^"

    testTrap.identified = false
    testTrap.displayChar shouldBe "."
  }

  behavior of "Dart Trap"

  it should "damage a target by a random amount" in {

    val target = new Goblin(new Position(1,1))
    target.health = 10

    testTrap.trigger(target)

    target.health shouldBe 7
  }

  it should "deal a minimum of 1 damage" in {
    implicit def mockRandImplicit(n: Int): Int = 0
    testTrap = DartTrap(new Position(0, 0))(mockRandImplicit)
    val target = new Goblin(new Position(1,1))
    target.health = 10

    testTrap.trigger(target)

    target.health shouldBe 9
  }
}

