package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Position.Position

abstract class Tile {

  val position: Position
  val id: String
  val displayChar: String
  val passable: Boolean = false

}

class EmptyTile(pos: Position) extends Tile {
  override val position = pos
  override val id = "EMPTY_TILE"
  override val displayChar = " "
  override val passable: Boolean = true
}

class FloorTile(pos: Position) extends Tile {
  override val position = pos
  override val id = "FLOOR_TILE"
  override val displayChar = " . "
  override val passable: Boolean = true
}

class VerticalWall(pos:Position) extends Tile {
  override val position = pos
  override val id = "VERTICAL_WALL"
  override val displayChar = " | "
}

class HorizontalWall(pos:Position) extends Tile {
  override val position = pos
  override val id = "HORIZONTAL_WALL"
  override val displayChar = "---"
}
