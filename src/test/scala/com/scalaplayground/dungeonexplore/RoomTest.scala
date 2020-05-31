package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Floor.Floor
import com.scalaplayground.dungeonexplore.Game.GameState
import net.team2xh.scurses.Scurses
import org.scalatest.{FlatSpec, Matchers}

class RoomTest extends FlatSpec with Matchers {
  behavior of "Room"

  it should "generate Cem Hial on level 5" in {
    var s = new Scurses
    val player = new Player("name", "class", "race")
    val gs = new GameState(player, s)
    gs.dungeonLevel = 6
    gs.floors = scala.collection.mutable.Seq[Floor](new Floor(1, false),new Floor(2, false),new Floor(3, false),new Floor(4, false),new Floor(5, true), new Floor(6, true))
    gs.createRooms

    gs.monsters.size shouldBe 1
    gs.monsters.head.name shouldBe "Cem Hial, the Necromancer"
    gs.rooms(1).containsCoords(gs.monsters.head.position)
  }
}
