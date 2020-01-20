package com.scalaplayground.dungeonexplore

import com.scalaplayground.dungeonexplore.Position.Position

import scala.util.Random

class Room(val startPosition: Position, val width: Int, val height: Int) {
  private val center = new Position(Math.ceil((startPosition.x + width + startPosition.x).toDouble / 2.toDouble).toInt,
                            Math.ceil((startPosition.y + height + startPosition.y).toDouble / 2.toDouble).toInt)

  def intersects(otherRoom: Room): Boolean = {
    return ((startPosition.x < otherRoom.startPosition.x + otherRoom.width) &&
            (startPosition.x + width > otherRoom.startPosition.x)) &&
            ((startPosition.y < otherRoom.startPosition.y + otherRoom.height) &&
            (startPosition.y + height > otherRoom.startPosition.y))
  }

  def getCenter: Position = {
    return center
  }

  def getRandomPosition: Position = {
    //start + rnd.nextInt( (end - start) + 1 )
    val x = startPosition.x + Random.nextInt( width + 1)
    val y = startPosition.y + Random.nextInt( height + 1)

    return new Position(x,y)
  }
}
