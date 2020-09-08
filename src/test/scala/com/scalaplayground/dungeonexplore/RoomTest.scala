package com.scalaplayground.dungeonexplore

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers


class RoomTest extends AnyFlatSpec with Matchers {
  behavior of "Room"

//  it should "generate Cem Hial on level 5" in {
//    var s = new Scurses
//    val player = new Player("name", "class", "race")
//    val gs = new GameState(player, s)
//    gs.dungeonLevel = 6
//    gs.floors = scala.collection.mutable.Seq[Floor](new Floor(1, false),new Floor(2, false),new Floor(3, false),new Floor(4, false),new Floor(5, true), new Floor(6, true))
//    gs.createRooms
//
//    gs.monsters.size mustBe 1
//    gs.monsters.head.name mustBe "Cem Hial, the Necromancer"
//    gs.rooms(1).containsCoords(gs.monsters.head.position)
//  }
}
