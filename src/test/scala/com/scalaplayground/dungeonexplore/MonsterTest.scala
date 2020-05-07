package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Monster._
import com.scalaplayground.dungeonexplore.Position.Position
//import com.scalaplayground.dungeonexplore.Position.Position
import org.scalatest.{FlatSpec, Matchers}

class MonsterTest extends FlatSpec
  with Matchers {
  behavior of "Monsters"
  behavior of "CemHial"

  it should "create a Cem Hial" in {
    val pos = new Position(0,0)
    val cem = CemHial(pos)
    cem.isAlive shouldBe true
  }

  it should "generate Cem Hial in a valid location" in {
    val room = Room(new Position(0,0), 4, 8)
    val pos = room.getRandomValidPosition
    val cem = new CemHial(new Position(pos.y, pos.x))

    room.containsCoords(cem.position) shouldBe true
  }
}
