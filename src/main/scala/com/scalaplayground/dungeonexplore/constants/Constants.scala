package com.scalaplayground.dungeonexplore.constants

object Constants {
  val MAGIC_ITEM_DROP_PERCENTAGE = 5
  val POTION_DROP_PERCENTAGE = 55
  val SCROLL_DROP_PERCENTAGE = 75
  val WEAPON_DROP_PERCENTAGE = 25
  val ARMOR_DROP_PERCENTAGE = 45
  val STARTING_PLAYER_HEALTH = 20
  val NUM_ROWS = 30
  val NUM_COLS = 100
  val MAX_MONSTERS_ALIVE = 5
  val MIN_ROOM_WIDTH = 5
  val MAX_ROOM_WIDTH = 10
  val MIN_ROOM_HEIGHT = 5
  val MAX_ROOM_HEIGHT = 8
  val MAX_NUM_ROOMS = 15
  val DEFAULT_SIGHT_DISTANCE = 4

  object Classes {
    val ALCHEMIST = "Alchemist"
    val WIZARD = "Wizard"
  }
}
