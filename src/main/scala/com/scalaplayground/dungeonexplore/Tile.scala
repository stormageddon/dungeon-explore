package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Position.Position

abstract class Tile {

  val position: Position
  val id: String
  var displayChar: String
  val passable: Boolean = false
  var occupied: Boolean = false
  var currentlyVisible: Boolean = false
  var hasBeenSeen: Boolean = false
  var description: String = "There is nothing here"
  var dist: Double = Double.MaxValue
  var neighbors: Seq[Vertex] = Seq[Vertex]()

  override def toString: String = {
    s"Tile: ${position.toString}"
  }

  def getNeighbors: Seq[Vertex] = {
    neighbors
  }
}

class Vertex(val tile: Tile, val weightedDist: Int) {
  override def toString(): String = {
    s"{tile: ${tile.position.toString} -> ${weightedDist}}"
  }
}

class EmptyTile(pos: Position, dispChar: String = " ") extends Tile {
  override val position = pos
  override val id = "EMPTY_TILE"
  override var displayChar = dispChar
  override val passable: Boolean = false
  description = "There is nothing here."
}

class FloorTile(pos: Position, dispChar: String = ".") extends Tile {
  override val position = pos
  override val id = "FLOOR_TILE"
  override var displayChar = dispChar
  override val passable: Boolean = true
  description = "A stone floor."
}

class VerticalWall(pos:Position, dispChar: String = "|") extends Tile {
  override val position = pos
  override val id = "VERTICAL_WALL"
  override var displayChar = dispChar
  description = "A damp, stone wall."
}

class HorizontalWall(pos:Position, dispChar: String = "---") extends Tile {
  override val position = pos
  override val id = "HORIZONTAL_WALL"
  override var displayChar = dispChar
  description = "A damp, stone wall."
}
